# Metaxis

Metaxis is a Haskell library for managing database migrations across multiple backends. It provides a unified interface for applying and rolling back migrations, supporting both SQLite and PostgreSQL.

## Features

- **Multi-backend support**: SQLite and PostgreSQL.
- **Migration management**: Apply and roll back migrations easily.
- **Schema tracking**: Keeps track of applied migrations in a dedicated table.

## Installation

To use Metaxis, ensure you have Haskell installed along with the required dependencies. 

### Build Flags

Metaxis supports optional build flags to enable specific database backends. Use `stack` to configure these flags during the build process:

```bash
# Build with SQLite enabled and PostgreSQL disabled
stack build --flag metaxis:sqlite --flag metaxis:-postgres

# Build with PostgreSQL enabled and SQLite disabled
stack build --flag metaxis:postgres --flag metaxis:-sqlite
```

If a backend is disabled, attempting to use it will result in an error message at runtime.

## Usage

### Commands

Metaxis provides two main commands:

1. **Migrate**: Apply all pending migrations.
2. **Rollback**: Roll back the last applied migration.

### Command-line Interface

Run the executable with the desired command and backend:

```bash
# Apply migrations using SQLite
metaxis migrate --backend sqlite

# Roll back the last migration using PostgreSQL
metaxis rollback --backend pg
```

### Migration Files

Migration files should be placed in the `migrations` directory. Each migration consists of:

- An `.sql` file for applying the migration.
- An optional `.down.sql` file for rolling back the migration.

#### Numbering Migrations

Migrations should be numbered sequentially to ensure proper ordering. While a three-digit format (e.g., `001`, `002`) is recommended for consistency, you can use any numbering scheme as long as it ensures migrations are applied in the correct order.

Example:

```
migrations/
├── 001_create_users.sql
├── 001_create_users.down.sql
├── 002_add_email_to_users.sql
├── 002_add_email_to_users.down.sql
```

### Backend Configuration

- **SQLite**: Uses a local file-based database. Default file is `dev.sqlite3`.
- **PostgreSQL**: Uses `defaultConnectInfo` for connection. Customize as needed.

## Running the Binary

After building the project, the `metaxis` executable will be located in the `.stack-work` directory. You can run it directly using the full path or add the directory to your `PATH` for convenience.

### Example Usage

Run the executable with the desired command and backend:

```bash
# Apply migrations using PostgreSQL
./.stack-work/install/aarch64-osx/<resolver>/bin/metaxis migrate --backend pg

# Roll back the last migration using SQLite
./.stack-work/install/aarch64-osx/<resolver>/bin/metaxis rollback --backend sqlite
```

Replace `<resolver>` with the specific resolver used during the build process (e.g., `cd0d45758eebe2f97dd3d906710943610beec0f500f409354d6ac8c1eeab2e1f/9.8.4`).

Alternatively, you can use `stack exec` to run the binary without specifying the full path:

```bash
stack exec metaxis -- migrate --backend pg
stack exec metaxis -- rollback --backend sqlite
```

## Development

To build the project, use the following commands:

```bash
stack build
stack exec metaxis -- <command>
```

## License

Metaxis is licensed under the MIT License.
