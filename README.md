# 🚗 Vehicle Register (Haskell)

A simple **CLI vehicle registry** written in Haskell. Add, search, update, and delete vehicles by registration number. Uses an in‑memory list with seed data and interactive prompts.

## ✨ Features
- List all vehicles (pretty printed)
- Search by registration number (exact match)
- Add with **duplicate RegNo protection**
- Update fields (model/year/regNo/status) with **blank = keep existing**
- Delete with **confirm guard**
- Seed data for quick start
- Minimal dependencies; works with Cabal or Stack

## 🛠 Tech Stack

<p align="left">
  <img src="https://cdn.jsdelivr.net/gh/devicons/devicon/icons/haskell/haskell-original.svg" alt="Haskell" width="40" height="40"/>
</p>

- **Language:** Haskell (Haskell2010)
- **Build:** Cabal v3 / Stack (LTS 22.x)
- **GHC:** 9.6+ recommended

## ▶️ Run

### Cabal
```bash
cabal v2-build
cabal v2-run vehicle-register
```

### Stack
```bash
stack build
stack run
```

## 🧪 Quick Demo
```
Loaded seed register with 5 vehicles.

====== Vehicle Register ======
1) List vehicles
2) Search by RegNo
3) Add vehicle
4) Update vehicle
5) Delete vehicle
6) Quit
Choose an option (1-6):
```

## 📁 Project Structure
```
.
├── src/Main.hs
├── vehicle-register.cabal
├── stack.yaml
├── README.md
├── LICENSE
├── .gitignore
└── .github/workflows/ci.yml
```

## 📝 Notes
- Status values accepted: `Registered` or `Unregistered` (case-insensitive).
- Year input is validated; prompts repeat on invalid input.
- Data is **in-memory** only (no persistence).

## 📜 License
MIT — see `LICENSE`.


## ✅ Testing

This project includes an **Hspec** test suite covering the pure helpers (`trim`, `lower`, `parseStatus`, `findVehicle`) and a QuickCheck-style property.

```bash
# Run tests (Cabal)
cabal v2-test

# Or with Stack
stack test
```
