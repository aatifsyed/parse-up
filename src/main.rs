use anyhow::bail;
use parse_up::{reedline::UpCompleter, ron};
use reedline::{
    default_emacs_keybindings, ColumnarMenu, DefaultPrompt, Emacs, KeyCode, KeyModifiers, Reedline,
    ReedlineEvent, ReedlineMenu, Signal,
};

fn main() -> anyhow::Result<()> {
    // let (completer, validator) = completer_and_validator(ron::value);
    let completion_menu = Box::new(ColumnarMenu::default().with_name("completion_menu"));
    // // Set up the required keybindings
    let mut keybindings = default_emacs_keybindings();
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );

    let edit_mode = Box::new(Emacs::new(keybindings));
    match Reedline::create()
        .with_completer(Box::new(UpCompleter::new(ron::float_int)))
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode)
        .read_line(&DefaultPrompt::default())?
    {
        Signal::CtrlC | Signal::CtrlD => bail!("input aborted"),
        Signal::Success(buffer) => {
            let result = ron::value(&buffer).expect("passed validation");
            println!("{result:?}");
        }
    }
    Ok(())
}
