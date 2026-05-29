# Накопленный контекст проекта friends.test

## Архитектура пакета

- **Строки** матрицы A — кандидаты в маркеры (гены, элементы).
- **Столбцы** — кандидаты в «друзья» (паттерны, клеточные типы, коллекции).
- Общий первый шаг: `row.int.ranks()` ранжирует каждый столбец независимо через `data.table::frankv(..., ties.method = "random")`.
- Два основных workflow: KS-тест (`friends.test`) и байесовский BIC (`friends.test.bic`).
- Оба возвращают одинаковый формат: вложенный именованный список, листья — `c(marker, friend, rank)`.
- Параллельность через `BiocParallel`, opt-in через параметр `BPPARAM`.

## Ключевые технические решения

### SnowParam и .libPaths() (май 2026)

**Проблема**: `R CMD build` устанавливает пакет во временную директорию,
добавляет её в `.libPaths()` родительского процесса, но SnowParam-воркеры
запускаются как свежие `Rscript`-процессы и эту директорию не наследуют.
Результат: `Error in loadNamespace(x): there is no package called 'friends.test'`
при сборке виньетки на Windows CI.

**Решение** (два компонента, оба необходимы):
1. `local(..., envir = globalenv())` — замыкание несёт `globalenv()` как среду,
   не пространство имён пакета → десериализация на воркере не требует загрузки пакета.
2. `libs = .libPaths()` передаётся через `MoreArgs`, воркер вызывает `.libPaths(libs)`
   перед любым `friends.test::` вызовом → воркер может найти пакет.

Реализовано в: `R/friends.test/R/friends.test.r`, `R/friends.test/R/friends.test.bic.r`.

### Иерархия классов BiocParallel (май 2026)

**Открытие**: `MulticoreParam` **наследует от `SnowParam`** в BiocParallel.
Следствие: `is(MulticoreParam(), "SnowParam")` == TRUE.
При проверке точного типа backend всегда использовать `class(BPPARAM) == "SnowParam"`,
а не `is(BPPARAM, "SnowParam")`.

### Тесты параллельности и dev-режим

`test_local()` / `pkgload::load_all()` загружает пакет только в память текущей сессии,
но не в `.libPaths()`. SnowParam-воркеры не могут его найти в dev-режиме.
Решение: `skip_if(pkgload::is_dev_package("friends.test"), ...)` в SnowParam-тестах.
MulticoreParam-тесты работают в dev-режиме (fork наследует память родителя),
но пропускаются на Windows: `skip_on_os("windows")`.

### Broken \link{} references

При удалении публичной функции из API нужно также убрать все `\link{ИМЯ}` в roxygen-комментариях
других файлов — иначе `R CMD check` даёт WARNING.

## Нерешённые задачи и известные тупики

### Красивый прогресс-бар для параллельных операций (май 2026)

**Цель**: заменить некрасивый `txtProgressBar` BiocParallel на что-то вроде `cli`/`progressr`.

**Попытка**: ветка `devel-fancy-progress` — интеграция `progressr::with_progress` +
`handler_cli()` с передачей `progressor` как аргумента в воркеры.

**Почему не сработало**:
1. Из-за наследования `MulticoreParam → SnowParam` условие `!is(BPPARAM, "SnowParam")`
   давало FALSE для MulticoreParam → progressr никогда не активировался.
2. После исправления на `class() != "SnowParam"`: прогресс-бар **не отображался**
   в VS Code R-терминале (не настоящий TTY; cli рендерит каждый update как новую строку).
3. Производительность: **5x замедление** на реальных данных. IPC-overhead от тысяч
   `p()` вызовов из форкнутых воркеров (каждый вызов пишет в соединение).
4. Компромисс (убрать `p()` из воркеров, вернуть BiocParallel bpprogressbar для всех):
   результат неотличим от `devel`, но чуть медленнее.

**Вывод**: передача `progressor` как аргумента в параллельные воркеры BiocParallel —
нежизнеспособный подход. Правильная интеграция progressr + BiocParallel требует
другого механизма. Ветка заброшена, остались на `devel`.

## Команды

```bash
# Из R/friends.test/
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --as-cran friends.test_0.99.19.tar.gz
Rscript -e 'BiocCheck::BiocCheck(".")'
```

## Целевое состояние тестов

FAIL 0 | WARN 0 | SKIP 2 (SnowParam в dev-режиме) | PASS 83

## Bioconductor

- Репо принимает полную историю коммитов — не нужен «один чистый коммит».
- Не переписывать историю после начала ревью.
- Ветка должна называться `devel` (не `master`).
- `~/friends.test` (master) — копия для Bioconductor; синхронизируется вручную.

## Версия

Текущая: **0.99.19**
