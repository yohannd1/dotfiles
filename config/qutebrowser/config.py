c.downloads.location.directory = "~/inbox"
bindings = [
    ('D', 'tab-close'),
    ('U', 'undo'),
    ('d', 'scroll-page 0 0.5'),
    ('u', 'scroll-page 0 -0.5'),
]

for kbind in bindings:
    config.bind(*kbind)
