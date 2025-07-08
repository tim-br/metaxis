# Metaxis

Metaxis is a Haskell library for managing database migrations across multiple backends. It provides a unified interface for applying and rolling back migrations, supporting both SQLite and PostgreSQL.

## Features

- **Multi-backend support**: SQLite and PostgreSQL.
- **Migration management**: Apply and roll back migrations easily.
- **Schema tracking**: Keeps track of applied migrations in a dedicated table.

## Installation

To use Metaxis, ensure you have Haskell installed along with the required dependencies. You can enable support for specific backends during build time using the appropriate flags:

- `POSTGRES_ENABLED` for PostgreSQL
- `SQLITE_ENABLED` for SQLite

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
├── 1_create_users.sql
├── 1_create_users.down.sql
├── 2_add_email_to_users.sql
├── 2_add_email_to_users.down.sql
```

### Backend Configuration

- **SQLite**: Uses a local file-based database. Default file is `dev.sqlite3`.
- **PostgreSQL**: Uses `defaultConnectInfo` for connection. Customize as needed.

## Development

To build the project, use the following commands:

```bash
stack build
stack exec metaxis -- <command>
```

## License

Metaxis is licensed under the MIT License.
