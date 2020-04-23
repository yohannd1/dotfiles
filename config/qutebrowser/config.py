c.downloads.location.directory = "~/inbox"

for bind_tuple in [('D', 'tab-close'),
                   ('U', 'undo'),
                   ('d', 'scroll-page 0 0.5'),
                   ('u', 'scroll-page 0 -0.5')]:
    config.bind(*bind_tuple)
