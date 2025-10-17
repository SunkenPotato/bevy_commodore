use bevy::{
    DefaultPlugins,
    app::{App, Startup, Update},
    camera::Camera2d,
    color::palettes::css::NAVY,
    ecs::{
        component::Component,
        message::{MessageReader, MessageWriter},
        query::With,
        resource::Resource,
        schedule::IntoScheduleConfigs,
        system::{Commands, In, Res, ResMut, Single},
    },
    math::Vec2,
    state::{
        app::AppExtStates,
        condition::in_state,
        state::{NextState, States},
    },
    ui::{AlignItems, BackgroundColor, FlexDirection, JustifyContent, Node, Val, widget::Text},
    utils::default,
};
use bevy_commodore::{
    Argument, CommandBuilder, CommandContext, CommandInput, CommandPlugin, CommandResult,
    OutputEvent, ParseError,
};
use bevy_ui_text_input::{
    SubmitText, TextInputMode, TextInputNode, TextInputPlugin, TextInputPrompt,
};

use std::{
    fmt::Write,
    ops::{Add, Div, Mul, Sub},
};

fn main() {
    let mut app = App::new();
    let plugin = CommandPlugin::<(), ()>::new();

    let mut echo_cmd = CommandBuilder::new(
        "echo",
        Some("Echo the input back to the user."),
        |In(mut ctx): In<CommandContext>| -> CommandResult {
            let input = ctx.argument::<String>("input");
            _ = writeln!(ctx, "{input}");

            CommandResult::Ok
        },
    );

    echo_cmd.with_argument::<String>("input", Some("The input to echo back"), false);

    let mut counter_cmd = CommandBuilder::new(
        "counter",
        Some("A simple, monotonic counter."),
        |_: In<CommandContext>| unreachable!(),
    );

    counter_cmd.with_subcommands(|builder| {
        builder
            .with_subcommand(
                "increment",
                Some("Increment the counter by one"),
                increment_counter,
                |_| (),
            )
            .with_subcommand(
                "get",
                Some("Display the value of the counter"),
                display_counter,
                |_| (),
            );
    });

    let mut math_cmd = CommandBuilder::new(
        "math",
        Some("Provides simple arithmetic operations"),
        |_: In<CommandContext>| unreachable!(),
    );

    math_cmd
        .with_subcommands(|builder| {
            builder
                .with_subcommand("add", None, math_command_handler_builder(i32::add), |_| ())
                .with_subcommand("sub", None, math_command_handler_builder(i32::sub), |_| ())
                .with_subcommand("mul", None, math_command_handler_builder(i32::mul), |_| ())
                .with_subcommand("div", None, math_command_handler_builder(i32::div), |_| ());
        })
        .with_argument::<i32>("lhs", Some("The left hand side of this operation"), false)
        .with_argument::<i32>("rhs", Some("The right hand side of this operation"), false);

    let mut distance_cmd = CommandBuilder::new(
        "distance",
        Some("Calculate the euclidean distance between two positions"),
        distance_cmd_handler,
    );

    distance_cmd
        .with_argument::<PositionArgumentType>("pos1", Some("The first position"), false)
        .with_argument::<PositionArgumentType>("pos2", Some("The second position"), false);

    plugin
        .register_command(echo_cmd)
        .register_command(counter_cmd)
        .register_command(math_cmd)
        .register_command(distance_cmd)
        .register_argument::<PositionArgumentType>()
        .with_default_args()
        .with_default_commands();

    app.add_plugins(DefaultPlugins).add_plugins(TextInputPlugin);
    app.add_plugins(plugin);
    app.init_state::<TextInputState>();

    app.add_systems(Startup, setup);
    app.add_systems(
        Update,
        (
            text_receiver.run_if(in_state(TextInputState::Reading)),
            command_output_writer.run_if(in_state(TextInputState::Processing)),
        ),
    );

    app.insert_resource(Counter(0));
    app.run();
}

fn distance_cmd_handler(In(mut ctx): In<CommandContext>) -> CommandResult {
    let pos1 = ctx.argument::<PositionArgumentType>("pos1");
    let pos2 = ctx.argument::<PositionArgumentType>("pos2");

    let distance =
        Vec2::new(pos1.x as f32, pos1.y as f32).distance(Vec2::new(pos2.x as f32, pos2.y as f32));
    _ = writeln!(
        ctx,
        "The distance between ({}; {}) and ({}; {}) is {}",
        pos1.x, pos1.y, pos2.x, pos2.y, distance
    );

    CommandResult::Ok
}

fn math_command_handler_builder(
    op: fn(i32, i32) -> i32,
) -> impl Fn(In<CommandContext>) -> CommandResult + Send + Sync + 'static {
    move |In(mut ctx): In<CommandContext>| {
        let lhs = *ctx.argument::<i32>("lhs");
        let rhs = *ctx.argument::<i32>("rhs");

        _ = writeln!(ctx, "{}", op(lhs, rhs));
        CommandResult::Ok
    }
}

#[derive(Resource)]
struct Counter(usize);

fn increment_counter(_: In<CommandContext>, mut counter: ResMut<Counter>) -> CommandResult {
    counter.0 += 1;
    CommandResult::Ok
}

fn display_counter(In(mut ctx): In<CommandContext>, counter: Res<Counter>) -> CommandResult {
    _ = writeln!(ctx, "Counter is at: {}", counter.0);
    CommandResult::Ok
}

#[derive(States, Clone, PartialEq, Eq, Hash, Debug, Default)]
enum TextInputState {
    #[default]
    Reading,
    Processing,
}

#[derive(Component)]
struct FeedbackMarker;

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);
    commands
        .spawn(Node {
            width: Val::Percent(100.),
            height: Val::Percent(100.),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            flex_direction: FlexDirection::Column,
            row_gap: Val::Px(10.),
            column_gap: Val::Px(20.),
            ..Default::default()
        })
        .with_child((
            TextInputNode {
                mode: TextInputMode::SingleLine,
                max_chars: Some(255),
                clear_on_submit: true,
                ..Default::default()
            },
            TextInputPrompt::new("Enter a command or `help`"),
            Node {
                width: Val::Px(500.),
                height: Val::Px(25.),
                ..default()
            },
            BackgroundColor(NAVY.into()),
        ))
        .with_child((Text::new(""), FeedbackMarker));
}

struct PositionArgumentType {
    x: i32,
    y: i32,
}

impl Argument for PositionArgumentType {
    fn parse(mut s: &str) -> Result<(&str, Self), bevy_commodore::ParseError>
    where
        Self: Sized,
    {
        if !s.starts_with('(') {
            return Err(ParseError::new(
                bevy_commodore::ParseErrorKind::InvalidSequence,
            ));
        }

        s = &s.trim()[1..];
        let (mut s, x) = i32::parse(s)?;
        s = s.trim();
        if !s.starts_with(';') {
            return Err(ParseError::new(
                bevy_commodore::ParseErrorKind::InvalidSequence,
            ));
        }

        s = &s.trim()[1..];
        let (mut s, y) = i32::parse(s)?;
        s = s.trim();
        if !s.starts_with(')') {
            return Err(ParseError::new(
                bevy_commodore::ParseErrorKind::InvalidSequence,
            ));
        }

        Ok((&s[1..], PositionArgumentType { x, y }))
    }

    fn type_name() -> &'static str
    where
        Self: Sized,
    {
        "position"
    }
}

fn text_receiver(
    mut text: MessageReader<SubmitText>,
    mut command_input: MessageWriter<CommandInput>,
    mut next_state: ResMut<NextState<TextInputState>>,
    mut text_input: Single<&mut TextInputNode>,
) {
    if let Some(text) = text.read().last() {
        command_input.write(CommandInput::new(text.text.clone()));
        text_input.is_enabled = false;
        next_state.set(TextInputState::Processing);
    }
}

fn command_output_writer(
    mut fdbk: MessageReader<OutputEvent>,
    mut output_field: Single<&mut Text, With<FeedbackMarker>>,
    mut text_input: Single<&mut TextInputNode>,
    mut next_state: ResMut<NextState<TextInputState>>,
) {
    use std::fmt::Write;

    if let Some(fdbk) = fdbk.read().last() {
        _ = match fdbk {
            OutputEvent::CommandError(cerr) => write!(output_field.0, "{cerr}"),
            OutputEvent::Ok(v) => write!(output_field.0, "{v}"),
            OutputEvent::RouterError(err) => match err {
                bevy_commodore::RouterError::Parse(parse) => {
                    writeln!(output_field.0, "Parse error: {:?}", parse.kind)
                }
                bevy_commodore::RouterError::UnknownCommand => {
                    writeln!(output_field.0, "Unknown command")
                }
                bevy_commodore::RouterError::UnknownSubcommand => {
                    writeln!(output_field.0, "Unknown subcommand")
                }
            },
            _ => unreachable!(),
        };

        next_state.set(TextInputState::Reading);
        text_input.is_enabled = true;
    }
}
