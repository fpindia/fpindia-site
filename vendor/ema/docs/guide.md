---
order: 3
---

# Guide

Writing an Ema app is an act in three parts:

1. Define your site [[route]] as Haskell ADTs:
   - Derive [[class]] for it
     - Optionally via [[generic]]
2. Define your site **data model** as a Haskell record:
   - [[model]] and [[dynamic]]
3. Connect it all using `EmaSite`
    - [[site]]
4. Optionally, [[compose|compose multiple apps]]

