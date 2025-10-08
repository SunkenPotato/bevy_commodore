//! A text command handler for the Bevy engine.
//!
//! The core API of this library is the [`CommandBuilder`] struct.

#![forbid(missing_docs)]
#![deny(clippy::missing_docs_in_private_items)]
#![warn(clippy::pedantic)]

pub mod builtins;
pub mod stored_sys;

use std::{
    any::{Any, TypeId},
    collections::{HashMap, HashSet},
    hash::Hash,
    marker::PhantomData,
    sync::{Arc, Mutex},
};

use bevy::{
    app::{App, Plugin, Update},
    ecs::{
        event::{Event, EventCursor, Events},
        resource::Resource,
        system::{In, IntoSystem, SystemId},
        world::World,
    },
};

use internment::Intern;

use derive_more::From;

use crate::{builtins::Identifier, stored_sys::StoredSystem};

/// A system representing a command handler.
pub type CommandHandler = StoredSystem<In<CommandContext>, CommandResult>;
/// A system representing a registered command handler. This differs from [`CommandHandler`] because it is a reference to a system, not the actual system.
pub type RegisteredCommandHandler = SystemId<In<CommandContext>, CommandResult>;
/// A stored argument parser.
type DynamicArgumentParser =
    Box<dyn Fn(&str) -> Result<(&str, Box<dyn Argument>), RouterError> + Send + Sync>;

/// A command plugin. This registers:
/// + all commands registered to this plugin
/// + events with the supplied markers, which are `()` by default
/// + a router system to handle input events.
///
/// Commands may be added with the [`CommandPlugin::register_command`]. \
/// Argument types may be added with the [`CommandPlugin::register_argument`].
pub struct CommandPlugin<I = (), O = ()> {
    /// Marker for the generic fields, which are only used to differentiate between these plugins.
    _p: PhantomData<(I, O)>,
    /// A set of commands.
    commands: Mutex<HashSet<CommandBuilder>>,
    /// The argument parsers.
    types: Mutex<HashMap<TypeId, ArgumentType>>,
}

/// The command registry. This stores pairs of command names and registered commands.
#[derive(Resource)]
pub struct CommandRegistry<I, O> {
    /// Marker for generic fields.
    _p: PhantomData<(I, O)>,
    /// The commands.
    commands: HashMap<Intern<str>, RegisteredCommand>,
}

/// The argument registry.
#[derive(Resource)]
pub struct ArgumentRegistry<I, O> {
    /// Marker for generic fields.
    _p: PhantomData<(I, O)>,
    /// Argument types.
    types: HashMap<TypeId, ArgumentType>,
}

/// An argument parser and the name of the argument type.
struct ArgumentType {
    /// The name.
    type_name: &'static str,
    /// The parser.
    parser: DynamicArgumentParser,
}

impl<I, O> Plugin for CommandPlugin<I, O>
where
    I: Send + Sync + 'static,
    O: Send + Sync + 'static,
{
    fn build(&self, app: &mut bevy::app::App) {
        app.add_event::<CommandInput<I>>();
        app.add_event::<OutputEvent<O>>();
        app.init_resource::<PluginEventCursor<I>>();

        let types = self.types.lock().unwrap().drain().collect();
        app.insert_resource::<ArgumentRegistry<I, O>>(ArgumentRegistry {
            _p: PhantomData,
            types,
        });

        let mut guard = self.commands.lock().unwrap();
        let commands = guard.drain().map(|v| (v.name, v.build(app))).collect();
        app.insert_resource::<CommandRegistry<I, O>>(CommandRegistry {
            _p: PhantomData,
            commands,
        });

        app.add_systems(Update, Self::router);
    }
}

/// Tracker for read events, since a system with exclusive [`World`] access cannot have `Local`s, which prevents storing the normal cursor.
#[derive(Resource)]
struct PluginEventCursor<I: Send + Sync + 'static>(EventCursor<CommandInput<I>>);

impl<I: Send + Sync + 'static> Default for PluginEventCursor<I> {
    fn default() -> Self {
        Self(EventCursor::default())
    }
}

impl<I, O> Default for CommandPlugin<I, O>
where
    I: Send + Sync + 'static,
    O: Send + Sync + 'static,
{
    fn default() -> Self {
        Self {
            commands: Mutex::new(HashSet::new()),
            types: Mutex::new(HashMap::new()),
            _p: PhantomData,
        }
    }
}

impl<I, O> CommandPlugin<I, O>
where
    I: Send + Sync + 'static,
    O: Send + Sync + 'static,
{
    /// Creates a new command plugin with no commands and no parsers.
    ///
    /// To add the default `help` command, call [`CommandPlugin::with_default_commands`]. \
    /// To add the default arguments, call [`CommandPlugin::with_default_arguments`].
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a command.
    #[expect(clippy::missing_panics_doc)]
    pub fn register_command(&self, command: CommandBuilder) -> &Self {
        self.commands.lock().unwrap().insert(command);
        self
    }

    /// Register an argument.
    #[expect(clippy::missing_panics_doc)]
    pub fn register_argument<A: Argument>(&self) -> &Self {
        let arg_type = ArgumentType {
            type_name: A::type_name(),
            parser: dynamic_parser::<A>(),
        };
        self.types
            .lock()
            .unwrap()
            .insert(TypeId::of::<A>(), arg_type);
        self
    }

    /// Register the "default" commands to this plugin.
    ///
    /// The default commands are:
    /// + `help`: Provides a description and syntax for the command and its nodes
    pub fn with_default_commands(&self) -> &Self {
        let mut help_cmd = CommandBuilder::new(
            "help",
            Some("Display help for all commands or the command name passed"),
            builtins::help_cmd_handler::<I, O>,
        );

        help_cmd.with_argument::<Identifier>(
            "command",
            Some("The command to retrieve help for."),
            true,
        );

        self.register_command(help_cmd)
    }

    // exclusive access to world to run the handlers
    #[allow(clippy::missing_docs_in_private_items)]
    fn router(world: &mut World) {
        world.resource_scope::<Events<CommandInput<I>>, _>(|world, events| {
            world.resource_scope::<PluginEventCursor<I>, _>(|world, mut cursor| {
                for event in cursor.0.read(&*events) {
                    Self::subrouter(event, world);
                }
            });
        });
    }

    #[allow(clippy::missing_docs_in_private_items)]
    fn subrouter(event: &CommandInput<I>, world: &mut World) {
        let input = event.s.as_str();
        let (name, body) = input.split_once(' ').unwrap_or((input, ""));
        let registry = world.resource::<CommandRegistry<I, O>>();
        let Some(command) = registry.commands.get(&name.into()) else {
            world
                .resource_mut::<Events<OutputEvent<O>>>()
                .send(OutputEvent::RouterError(RouterError::UnknownCommand));
            return;
        };

        let arg_registry = world.resource::<ArgumentRegistry<I, O>>();
        let args = match command.parse(arg_registry, body) {
            Ok(v) => v.1,
            Err(e) => {
                world
                    .resource_mut::<Events<OutputEvent<O>>>()
                    .send(OutputEvent::RouterError(e));
                return;
            }
        };

        let sink = Arc::new(Mutex::new(String::new()));
        let ctx = CommandContext {
            sink: Arc::clone(&sink),
            arguments: args,
        };

        let result = world.run_system_with(command.handler, ctx).unwrap();
        let string = Arc::into_inner(sink).unwrap().into_inner().unwrap();
        let output = match result {
            CommandResult::Ok => OutputEvent::Ok(string),
            CommandResult::Err => OutputEvent::CommandError(string),
        };

        world.resource_mut::<Events<OutputEvent<O>>>().send(output);
    }
}

/// A text supplier for the command plugin.
#[derive(Event)]
pub struct CommandInput<I = ()> {
    /// The input string.
    s: String,
    /// Marker for generics.
    _p: PhantomData<I>,
}

impl<I> CommandInput<I> {
    /// Construct a new [`CommandInput`] from a [`String`].
    #[must_use]
    pub const fn new(s: String) -> Self {
        Self { s, _p: PhantomData }
    }
}

/// Indicates the output and result of a command, sent by the command plugin.
#[derive(Event, Debug)]
pub enum OutputEvent<O = ()> {
    /// The command was successful.
    Ok(String),
    /// The command was unsuccessful.
    CommandError(String),
    /// The router encountered an error.
    RouterError(RouterError),
    #[doc(hidden)]
    __Private(PhantomData<O>, std::convert::Infallible),
}

impl<O> OutputEvent<O> {
    /// Retrieve the string this command returned. If this is a [`Self::RouterError`], this returns `None`.
    #[must_use]
    pub fn content(&self) -> Option<&str> {
        match self {
            Self::Ok(s) | Self::CommandError(s) => Some(s),
            Self::RouterError(_) => None,
            Self::__Private { .. } => unreachable!(),
        }
    }
}

/// An error returned by the command router.
#[derive(Debug, From, PartialEq, Eq)]
pub enum RouterError {
    /// The input could not be parsed.
    Parse(ParseError),
    /// The router could not find the passed command.
    UnknownCommand,
    /// The router could not find the passed subcommand.
    UnknownSubcommand,
}

/// A parser error kind.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParseErrorKind {
    /// An invalid sequence was found.
    InvalidSequence,
    /// The input was input.
    Empty,
    /// The input would have overflown.
    Overflow,
}

/// A parser error.
#[derive(Debug)]
pub struct ParseError {
    /// The parser error kind.
    pub kind: ParseErrorKind,
    /// The cause for this error. This can be anything, it is up to the library user.
    pub cause: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
impl Eq for ParseError {}

impl ParseError {
    /// Create a new [`ParseError`] with the given [`ParseErrorKind`].
    #[must_use]
    pub const fn new(kind: ParseErrorKind) -> Self {
        Self { kind, cause: None }
    }

    /// Provide a cause for this error.
    #[must_use]
    pub fn with_cause(self, cause: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self {
            cause: Some(Box::new(cause)),
            ..self
        }
    }
}

/// A command result.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(u8)]
pub enum CommandResult {
    /// The command was successful.
    #[default]
    Ok = 1,
    /// The command was unsuccessful.
    Err = 0,
}

/// Defines a parser for argument types, as well as providing downcast for them.
pub trait Argument: Any {
    /// Parse the given argument.
    ///
    /// # Errors
    /// This should return an error if parsing failed.
    fn parse(s: &str) -> Result<(&str, Self), ParseError>
    where
        Self: Sized;

    /// A friendly, user-facing type name. Usually, the default implementation ([`std::any::type_name`]) is sufficient, however,
    /// implementors should consider using a different one for types whose names are programmer-facing.
    #[must_use]
    fn type_name() -> &'static str
    where
        Self: Sized,
    {
        std::any::type_name::<Self>()
    }
}

/// A parsed argument instance.
type ArgumentInstance = Box<dyn Argument + 'static>;

/// A command argument definition.
pub struct ArgumentDefinition {
    /// The name.
    name: Intern<str>,
    /// The description.
    description: Option<Intern<str>>,
    /// The type ID of the argument. Cross-referenced with the `ArgumentRegistry` to obtain a parser.
    type_id: TypeId,
    /// Whether this argument is optional when parsing.
    optional: bool,
}

/// A command context, passed to a command handler to provide arguments and an output sink.
pub struct CommandContext {
    /// The arguments to this command.
    arguments: HashMap<Intern<str>, ArgumentInstance>,
    /// The output sink, which will be converted into an [`OutputEvent`].
    sink: Arc<Mutex<String>>,
}

impl CommandContext {
    /// Get an argument by name. The type must also be supplied.
    ///
    /// # Panics
    /// This panics if the there was no argument matching the type or name.
    pub fn argument<A: Argument>(&mut self, name: impl Into<Intern<str>>) -> Box<A> {
        self.get_argument(name).unwrap()
    }

    /// Get an argument by name and type.
    /// This returns `None` if no such argument was found.
    pub fn get_argument<A: Argument>(&mut self, name: impl Into<Intern<str>>) -> Option<Box<A>> {
        let name = name.into();
        #[expect(clippy::missing_panics_doc)]
        if let Some(arg) = self.arguments.get(&name)
            && (**arg).type_id() == TypeId::of::<A>()
        {
            let arg: Box<dyn Any> = self.arguments.remove(&name).unwrap();
            Some(arg.downcast::<A>().unwrap())
        } else {
            None
        }
    }
}

impl std::fmt::Write for CommandContext {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        write!(self.sink.lock().unwrap(), "{s}")
    }
}

/// A command node, representing either an argument or a group of subcommands.
enum CommandNode {
    /// An argument.
    Argument(ArgumentDefinition),
    /// A group of subcommands.
    SubcommandGroup(Vec<CommandBuilder>),
}

/// A subcommand group builder.
#[derive(Default)]
pub struct SubcommandGroupBuilder(Vec<CommandBuilder>);

impl SubcommandGroupBuilder {
    /// Add a subcommand to this group.
    ///
    /// + `name`: the name of this command
    /// + `description`: an optional description for this command
    /// + `handler`: the handler system for this command. This must take an `In<CommandContext>`, at least
    /// + `builder_f`: The builder function for this subcommand. This is where you add arguments (or even further subcommands) to this subcommand
    pub fn with_subcommand<H, B, M>(
        &mut self,
        name: &str,
        description: Option<&str>,
        handler: H,
        builder_f: B,
    ) -> &mut Self
    where
        B: FnOnce(&mut CommandBuilder),
        H: IntoSystem<In<CommandContext>, CommandResult, M> + Send + Sync + 'static,
    {
        let mut builder = CommandBuilder::new(name, description, handler);
        builder_f(&mut builder);
        self.0.push(builder);
        self
    }
}

/// A command builder.
///
/// The order that command nodes (argument or subcommand group) are added with their respective `with_` methods determines the order they are parsed in,
/// and therefore the layout of the command.
pub struct CommandBuilder {
    /// The name of this command.
    name: Intern<str>,
    /// The description of this command.
    description: Option<Intern<str>>,
    /// The handler for this command.
    handler: CommandHandler,
    /// The nodes on this command.
    nodes: Vec<CommandNode>,
}

impl CommandBuilder {
    /// Create a new command.
    ///
    /// + `name`: The name of this command
    /// + `description`: An optional description for this subcommand
    /// + `handler`: The handler system for this command. This must be a system that takes at minimum an `In<CommandContext>`
    pub fn new<S, M>(name: &str, description: Option<&str>, handler: S) -> Self
    where
        S: IntoSystem<In<CommandContext>, CommandResult, M> + Send + Sync + 'static,
    {
        Self {
            name: name.into(),
            description: description.map(Into::into),
            handler: StoredSystem::new(handler),
            nodes: vec![],
        }
    }

    /// Add an argument node to this command. Arguments are stored - and therefore parsed - in the order that they are passed to this function.
    pub fn with_argument<T>(
        &mut self,
        name: &str,
        description: Option<&str>,
        optional: bool,
    ) -> &mut Self
    where
        T: Argument,
    {
        self.nodes.push(CommandNode::Argument(ArgumentDefinition {
            name: name.into(),
            description: description.map(Into::into),
            type_id: TypeId::of::<T>(),
            optional,
        }));

        self
    }

    /// Add a subcommand group node to this command.
    ///
    /// This method takes a closure which is passed a `&mut SubcommandGroupBuilder`. The additions made to said builder are used for the subcommand.
    ///
    /// # Panics
    /// This method panics if no subcommands were added to the [`SubcommandGroupBuilder`].
    pub fn with_subcommands<B>(&mut self, builder: B) -> &mut Self
    where
        B: FnOnce(&mut SubcommandGroupBuilder),
    {
        let mut scg_builder = SubcommandGroupBuilder::default();
        builder(&mut scg_builder);
        assert!(
            !scg_builder.0.is_empty(),
            "At least one subcommand is required"
        );
        self.nodes.push(CommandNode::SubcommandGroup(scg_builder.0));
        self
    }

    /// Register this subcommand to the given app.
    fn build(self, app: &mut App) -> RegisteredCommand {
        let handler = self.handler.register(app);
        let nodes = self
            .nodes
            .into_iter()
            .map(|v| match v {
                CommandNode::Argument(a) => RegisteredCommandNode::Argument(a),
                CommandNode::SubcommandGroup(scg) => RegisteredCommandNode::SubcommandGroup(
                    scg.into_iter().map(|v| v.build(app)).collect(),
                ),
            })
            .collect();

        RegisteredCommand {
            name: self.name,
            description: self.description,
            handler,
            nodes,
        }
    }
}

impl PartialEq for CommandBuilder {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for CommandBuilder {}
impl Hash for CommandBuilder {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

/// Create a type-erased parser from a given type parameter.
fn dynamic_parser<T: Argument>() -> DynamicArgumentParser {
    Box::new(move |s| -> Result<(&str, Box<dyn Argument>), RouterError> {
        T::parse(s)
            .map(|(s, t)| {
                let boxed: Box<dyn Argument> = Box::new(t);
                (s, boxed)
            })
            .map_err(Into::into)
    })
}

/// A registered command node (definition). This may be an argument definition or a subcommand group.
enum RegisteredCommandNode {
    /// An argument definition.
    Argument(ArgumentDefinition),
    /// A subcommand group.
    SubcommandGroup(Vec<RegisteredCommand>),
}

/// A command that has been registered to an [`App`].
pub struct RegisteredCommand {
    /// The name of this command.
    name: Intern<str>,
    /// The description of this command.
    description: Option<Intern<str>>,
    /// The handler of this command.
    handler: RegisteredCommandHandler,
    /// The nodes on this command.
    nodes: Vec<RegisteredCommandNode>,
}

impl RegisteredCommand {
    /// Parse a registered command, according to its node layout.
    fn parse<'b, I, O>(
        &self,
        arg_registry: &ArgumentRegistry<I, O>,
        mut body: &'b str,
    ) -> Result<(&'b str, HashMap<Intern<str>, ArgumentInstance>), RouterError> {
        let mut args = HashMap::new();

        for node in &self.nodes {
            let rest = match node {
                RegisteredCommandNode::Argument(def) => {
                    let parser = &arg_registry.types.get(&def.type_id).unwrap().parser;
                    let (rest, arg) = match (def.optional, parser(body)) {
                        (true, Err(_)) => continue,
                        (false, Err(e)) => return Err(e),
                        (_, Ok((rest, arg))) => (rest, arg),
                    };
                    args.insert(def.name, arg);
                    rest
                }
                RegisteredCommandNode::SubcommandGroup(scg) => {
                    let (rest, ident) = Identifier::parse(body)?;

                    match scg.iter().find(|v| v.name == ident.0) {
                        Some(v) => {
                            let (rest, sc_args) = v.parse(arg_registry, rest)?;
                            args.extend(sc_args);
                            rest
                        }
                        None => return Err(RouterError::UnknownSubcommand),
                    }
                }
            };

            body = rest.trim();
        }

        Ok((body, args))
    }
}
