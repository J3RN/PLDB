# The Programming Language Database (PLDB)

This is a database of programming languages, built using [Dhall](https://dhall-lang.org/).  The data is stored in the `pldb.dhall` file.

To convert the data to JSON, you will need the `dhall-to-json` tool.  You can install it from the [latest Dhall release](https://github.com/dhall-lang/dhall-haskell/releases/latest) (or, if you are using a mac, from Homebrew with `brew install dhall-to-json`).

Once you have `dhall-to-json`, you can run the following command to convert the data to JSON:

```
dhall-to-json --file pldb.dhall --output pldb.json
```

This will create a `pldb.json` file with the data in JSON format.

## Errata

### No "Procedural" Paradigm

Programming language paradigms are rather loosely defined.  Wikipedia describes a "procedural" language as an imperative language with block structure like `if`, `while`, etc.  So far as I'm aware, this definition includes every imperative language after ALOGOL 58; the only imperative language in PLDB that does not qualify is Assembly.  Therefore, the distinction did not seem worthwhile.

## Contributing

If you want to contribute to this database, please feel free to submit a pull request!  I know that there are many languages, resources, etc that I have missed, and I would love to have help adding them.

If you have any questions, please feel free to open an issue.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
