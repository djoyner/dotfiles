(autoload 'twitter-get-friends-timeline "twitter" "\
Fetch and display the friends timeline.
The results are formatted and displayed in a buffer called
*Twitter friends timeline*

If the variable `twitter-include-replies' is non-nil, the replies
timeline will also be merged into the friends timeline and
displayed." t nil)

(autoload 'twitter-status-edit "twitter" "\
Edit your twitter status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[twitter-status-post] when you are finished editing to send the
message." nil nil)

(provide 'twitter-config)
