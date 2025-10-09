# bevy_commodore
This is a text command framework for [Bevy](https://github.com/bevyengine/bevy), largely inspired by Minecraft's
[Brigadier command parser & dispatcher](https://github.com/Mojang/brigadier).

## Features
+ Multiple simultaneous command and argument registries
+ Custom argument types
+ Optional arguments
+ (Nested) subcommands

## Getting started
To start, simply register your commands to your `CommandPlugin` plugin and add it to the app, e.g.:
```rs
use bevy::{DefaultPlugins, app::App};
use bevy_commodore::{CommandBuilder, CommandContext, CommandPlugin, CommandResult};

use std::fmt::Write;

fn echo_command_handler(In(mut ctx): In<CommandContext>) -> CommandResult {
    let input = *tx.argument::<String>("input");
    _ = writeln!(ctx, "{input}");

    CommandResult::Ok
}

let mut app = App::new();
let plugin = CommandPlugin::<(), ()>::new();

let echo_command = CommandBuilder::new("echo", Some("Echoes the input text back to the user"), echo_command_handler);
echo_command.with_argument::<String>("input", None, false);

plugin.register_command(plugin);

app.add_plugins(DefaultPlugins);
app.add_plugins(plugin);

app.run();
```

> [!NOTE]
> This example will work, but it will provide no way to call these commands. To supply input to the command
plugin, use the `CommandInput` event. You'll be able to get the result of the command via the `CommandOutput` event.

For a more detailed and fully functional example showcasing more features, you can view the `examples/example.rs` file.

## Roadmap
- [ ] Variadic arguments
- [ ] Help for subcommands
