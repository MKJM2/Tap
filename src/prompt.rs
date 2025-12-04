use chrono::Utc;
use reedline::{Prompt as ReedlinePrompt, PromptEditMode, PromptHistorySearch};
use std::borrow::Cow;

#[derive(Clone)]
pub struct Prompt;

impl ReedlinePrompt for Prompt {
    fn render_prompt_left(&self) -> std::borrow::Cow<'_, str> {
        Cow::Borrowed("tap")
    }

    fn render_prompt_right(&self) -> std::borrow::Cow<'_, str> {
        // Render current time
        let time_str = Utc::now().format("%H:%M:%S").to_string();
        Cow::Owned(time_str)
    }

    fn render_prompt_indicator(&self, _edit_mode: PromptEditMode) -> Cow<'_, str> {
        Cow::Owned(String::from(" >> "))
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Owned(String::from(" multiline >> "))
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        Cow::Owned(String::from(" search history >> "))
    }
}
