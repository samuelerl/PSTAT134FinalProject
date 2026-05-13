# Chat UI fragment — static placeholder transcript + composer (design shell).

chat_ui <- function() {
  tagList(
    div(
      class = "chat-app",
      div(
        class = "chat-thread",
        div(
          class = "chat-message chat-message--assistant",
          div(class = "chat-message-label", "Assistant"),
          div(
            class = "chat-message-body",
            paste(
              "Hi — I'm your movie recommender assistant (UI preview).",
              "When the model is connected, you'll get personalized picks here."
            )
          )
        ),
        div(
          class = "chat-message chat-message--user",
          div(class = "chat-message-label", "You"),
          div(
            class = "chat-message-body",
            "Something like The Matrix but more recent — what should I watch tonight?"
          )
        ),
        div(
          class = "chat-message chat-message--assistant",
          div(class = "chat-message-label", "Assistant"),
          div(
            class = "chat-message-body",
            paste(
              "Placeholder reply: I'd look at sci-fi thrillers from the last few years",
              "and narrow by streaming availability. (Wire your recommender output here.)"
            )
          )
        )
      ),
      div(
        class = "chat-composer",
        div(
          class = "chat-composer-actions",
          div(
            style = "flex: 1; min-width: 0;",
            textAreaInput(
              inputId = "chat_input",
              label = NULL,
              placeholder = "Message the assistant…",
              rows = 2
            )
          ),
          actionButton("send", "Send", class = "btn-primary")
        ),
        helpText(
          "Model not connected — design preview only.",
          class = "chat-composer-hint"
        )
      )
    )
  )
}
