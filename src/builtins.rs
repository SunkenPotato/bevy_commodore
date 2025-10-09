//! Built-in argument types and commands.
//!
//! The [`args`] module provides an implementation of the [`Argument`](crate::Argument) trait for a few, commonly used argument types, such as [`f32`], [`String`], [`Identifier`](args::Identifier), and [`i32`]. \
//! The [`cmd`] module provides implementations of command handlers for a few built-in commands.

/// Built-in argument types
pub mod args {
    use bevy::prelude::Deref;
    use internment::Intern;

    use crate::Argument;

    /// An identifier. This is an unquoted string that matches the regex:
    /// ```regex
    /// ^[a-zA-Z_][a-zA-Z0-9_]*
    /// ```
    #[repr(transparent)]
    #[derive(Clone, Copy, PartialEq, Eq, Hash, Deref)]
    pub struct Identifier(pub Intern<str>);

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

        fn type_name() -> &'static str
        where
            Self: Sized,
        {
            "identifier"
        }
    }

    impl Argument for String {
        fn parse(s: &str) -> Result<(&str, Self), crate::ParseError>
        where
            Self: Sized,
        {
            let s = s.trim_start();

            // Empty input?
            if s.is_empty() {
                return Err(crate::ParseError::new(crate::ParseErrorKind::Empty));
            }

            // Quoted string case
            if let Some(rest) = s.strip_prefix('"') {
                let mut result = String::new();
                let mut chars = rest.char_indices();
                let mut escape = false;

                for (i, c) in &mut chars {
                    if escape {
                        // Handle escaped characters
                        match c {
                            '"' => result.push('"'),
                            '\\' => result.push('\\'),
                            'n' => result.push('\n'),
                            't' => result.push('\t'),
                            other => result.push(other),
                        }
                        escape = false;
                        continue;
                    }

                    match c {
                        '\\' => escape = true,
                        '"' => {
                            // End of quoted string
                            let rest = &rest[i + 1..];
                            return Ok((rest.trim_start(), result));
                        }
                        _ => result.push(c),
                    }
                }

                // If we reach here, the closing quote was missing
                Err(
                    crate::ParseError::new(crate::ParseErrorKind::InvalidSequence)
                        .with_cause("missing closing quote".to_owned()),
                )
            } else {
                // Unquoted string: read until next whitespace
                let end = s.find(char::is_whitespace).unwrap_or(s.len());
                let word = &s[..end];
                let rest = &s[end..];
                Ok((rest.trim_start(), word.to_string()))
            }
        }

        fn type_name() -> &'static str
        where
            Self: Sized,
        {
            "string"
        }
    }

    impl Argument for i32 {
        fn parse(s: &str) -> Result<(&str, Self), crate::ParseError>
        where
            Self: Sized,
        {
            let s = s.trim_start();
            if s.is_empty() {
                return Err(crate::ParseError::new(crate::ParseErrorKind::Empty));
            }

            // Find where the integer token ends
            let mut end = 0;
            let mut chars = s.char_indices();

            // Optional leading sign
            if let Some((_, c)) = chars.next() {
                if c == '-' || c == '+' || c.is_ascii_digit() {
                    end = 1;
                } else {
                    return Err(crate::ParseError::new(
                        crate::ParseErrorKind::InvalidSequence,
                    ));
                }
            }

            // Consume all following digits
            for (i, c) in chars {
                if c.is_ascii_digit() {
                    end = i + c.len_utf8();
                } else {
                    break;
                }
            }

            let number_str = &s[..end];
            let rest = &s[end..];

            match number_str.parse::<i32>() {
                Ok(num) => Ok((rest.trim_start(), num)),
                Err(_) => Err(crate::ParseError::new(
                    crate::ParseErrorKind::InvalidSequence,
                )),
            }
        }

        fn type_name() -> &'static str
        where
            Self: Sized,
        {
            "integer"
        }
    }
}

/// Implementations for default commands.
pub mod cmd {
    use bevy::ecs::system::{In, Res};
    use itertools::Itertools;

    use crate::{
        ArgumentRegistry, CommandContext, CommandRegistry, CommandResult, RegisteredCommand,
        RegisteredCommandNode, builtins::args::Identifier,
    };

    /// The command handler for the built-in `help` function.
    #[expect(clippy::needless_pass_by_value)]
    #[expect(clippy::must_use_candidate)]
    pub fn help_cmd_handler<I, O>(
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
                    _ = write!(ctx, " [{}]", scg.iter().map(|v| v.name).join("|"));
                }
            }
        }

        if let Some(desc) = command.description {
            _ = writeln!(ctx, ": {desc}");
        }

        if command
            .nodes
            .iter()
            .any(|v| matches!(v, RegisteredCommandNode::Argument(_)))
        {
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
        _ = writeln!(ctx);
    }
}
