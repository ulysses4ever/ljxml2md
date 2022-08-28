## ljxml2md: turn your livejournal into static blog

The tagline says all of it.

### Running

It's a Haskell executable package, which can optionally be built with Nix. E.g. run the 
app on a test XML from `test/L-1`:

```shell
❯ nix-shell --run 'cat test/L-1 | runhaskell ljxml2md.hs'
---
abs_id: 1
anum: 212
current_moodid: 107
event_timestamp: 1166134500
eventtime: "2006-12-14 22:15:00"
lj_url_id: 468
oldurl: "https://ulysses4ever.livejournal.com/468.html"
reply_count: 0
taglist: livejournal, welcome
title: Добро пожаловать в Пропасть во ржи!
---

Надеюсь, будет интересно.
```

