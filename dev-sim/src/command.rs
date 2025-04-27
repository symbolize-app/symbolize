#[derive(Clone, Copy, Debug)]
pub struct CommandDefinition {
  pub id: CommandId,
  pub name: &'static str,
  pub aliases: &'static [&'static str],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommandId {
  Begin,
  Connect,
  Disconnect,
  End,
  Help,
  Ping,
  Quit,
  Sleep,
  Tempo,
  Use,
}

pub const COMMANDS: &[CommandDefinition] = &[
  CommandDefinition {
    id: CommandId::Begin,
    name: "begin",
    aliases: &["b"],
  },
  CommandDefinition {
    id: CommandId::Connect,
    name: "connect",
    aliases: &["c"],
  },
  CommandDefinition {
    id: CommandId::Disconnect,
    name: "disconnect",
    aliases: &["d"],
  },
  CommandDefinition {
    id: CommandId::End,
    name: "end",
    aliases: &["e"],
  },
  CommandDefinition {
    id: CommandId::Help,
    name: "help",
    aliases: &["h"],
  },
  CommandDefinition {
    id: CommandId::Ping,
    name: "ping",
    aliases: &["p"],
  },
  CommandDefinition {
    id: CommandId::Quit,
    name: "quit",
    aliases: &["q"],
  },
  CommandDefinition {
    id: CommandId::Sleep,
    name: "sleep",
    aliases: &["s"],
  },
  CommandDefinition {
    id: CommandId::Tempo,
    name: "tempo",
    aliases: &["t"],
  },
  CommandDefinition {
    id: CommandId::Use,
    name: "use",
    aliases: &["u"],
  },
];

#[derive(Clone, Copy, Debug)]
pub enum CommandParameters {
  Begin {},
  Connect {},
  Disconnect {},
  End {},
  Help {},
  Ping {},
  Quit {},
  Sleep {},
  Tempo {},
  Use {},
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommandStatus {
  Continue,
  Quit,
}
