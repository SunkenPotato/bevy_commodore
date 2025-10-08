//! Built-in argument types.
//!
//! This crate provides an implementation of the [`Parse`] trait for a few, commonly used argument types, such as [`f32`], [`String`], [`Identifier`], and [`i32`].

use std::ops::Deref;

use bevy::ecs::system::{In, Res};
use internment::Intern;
use itertools::Itertools;

use crate::{
    Argument, ArgumentRegistry, CommandContext, CommandRegistry, CommandResult, RegisteredCommand,
    RegisteredCommandNode,
};

/// An identifier. This is an unquoted string that matches the regex:
/// ```regex
/// ^[a-zA-Z_][a-zA-Z0-9_]*
/// ```
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Identifier(pub Intern<str>);

impl Deref for Identifier {
    type Target = Intern<str>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Argument for Identifier {
    fn parse(s: &str) -> Result<(&str, Self), crate::ParseError>
    where
        Self: Sized,
    {
        let mut chars = s.char_indices();

        let first = match chars.next() {
            Some((_, c)) if c.is_ascii_alphabetic() || c == '_' => c,
            _ => {
                return Err(crate::ParseError::new(
                    crate::ParseErrorKind::InvalidSequence,
                ));
            }
        };

        let mut end = first.len_utf8();
        for (i, c) in chars {
            if c.is_ascii_alphanumeric() || c == '_' {
                end = i + c.len_utf8();
            } else {
                break;
            }
        }

        let ident = &s[..end];
        let rest = &s[end..];

        Ok((rest, Identifier(ident.into())))
    }
}

/// The command handler for the built-in `help` function.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn help_cmd_handler<I, O>(
    In(mut ctx): In<CommandContext>,
    commands: Res<CommandRegistry<I, O>>,
    arg_reg: Res<ArgumentRegistry<I, O>>,
) -> CommandResult
where
    I: Send + Sync + 'static,
    O: Send + Sync + 'static,
{
    use std::fmt::Write;

    if let Some(cmd) = ctx.get_argument::<Identifier>("command") {
        if let Some(cmd_def) = commands.commands.get(&cmd) {
            write_cmd_info(&mut ctx, cmd_def, &*arg_reg);
        } else {
            _ = writeln!(ctx, "Command \"{}\" does not exist", &**cmd);
            return CommandResult::Err;
        }
    } else {
        for command in &commands.commands {
            write_cmd_info(&mut ctx, command.1, &*arg_reg);
        }
    }

    CommandResult::Ok
}

/// Write info about a command to `ctx`. This is used for the `help`-command handler.
fn write_cmd_info<I, O>(
    ctx: &mut CommandContext,
    command: &RegisteredCommand,
    arg_reg: &ArgumentRegistry<I, O>,
) {
    use std::fmt::Write;

    _ = write!(ctx, "{}", command.name);

    for node in &command.nodes {
        match node {
            RegisteredCommandNode::Argument(arg) => {
                _ = write!(
                    ctx,
                    " <{}: {}>",
                    arg.name,
                    arg_reg.types.get(&arg.type_id).unwrap().type_name
                );
            }
            RegisteredCommandNode::SubcommandGroup(scg) => {
                _ = write!(ctx, "[{}]", scg.iter().map(|v| v.name).join("|"));
            }
        }
    }

    if let Some(desc) = command.description {
        _ = writeln!(ctx, ": {desc}");
    }

    if !command.nodes.is_empty() {
        _ = writeln!(ctx, "Arguments: ");
    }

    for args in command.nodes.iter().filter_map(|v| {
        if let RegisteredCommandNode::Argument(arg) = v {
            Some(arg)
        } else {
            None
        }
    }) {
        _ = writeln!(
            ctx,
            "\t- {}: {}",
            args.name,
            args.description.map_or("", |v| v.as_ref())
        );
    }
}
