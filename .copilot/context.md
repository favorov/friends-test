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
другого механизма. Ветка `devel-fancy-progress` заброшена, работа слита в `devel`.

### Архитектура progress bar в коде (важно для будущей работы)

**`ft_bpparam()` в `R/biocparallel-utils.r`:**
```r
ft_bpparam <- function(BPPARAM = NULL, .progress = FALSE) {
    if (is.null(BPPARAM)) BPPARAM <- BiocParallel::SerialParam()
    BiocParallel::bpprogressbar(BPPARAM) <- FALSE  # всегда FALSE
    BPPARAM
}
```
Параметр `.progress` принимается, но на `bpprogressbar` не влияет — оставлено намеренно.
Чтобы включить BiocParallel-бар: изменить `FALSE` →
`if (.progress && !is(BPPARAM, "SerialParam")) TRUE else FALSE`
и добавить `BiocParallel::bptasks(BPPARAM) <- n_rows` (число строк матрицы),
чтобы бар двигался по одному элементу, а не скачками на число воркеров.
BiocParallel внутри делает `min(tasks, length(X))`, поэтому `.Machine$integer.max`
тоже безопасен как sentinel — но лучше передавать реальное число.
`ft_bpparam()` вызывается до диспетчеризации и не знает числа строк — его нужно
передавать отдельно (или менять сигнатуру функции).

**`use_serial_progress` в `friends.test.r` и `friends.test.bic.r`:**
```r
use_serial_progress <- .progress && is(BPPARAM, "SerialParam")
```
- `TRUE` → `cli_progress_along()` с живым баром и `format_done` с elapsed time
- `FALSE` → только `cli_progress_step()` (текстовая метка без динамики)

**`cli_progress_along` format_done (SerialParam):**
```r
cli::cli_progress_along(
    X, name = "...", clear = FALSE,
    format_done = "{cli::pb_name}{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed}"
)
```

**Проблема пустой строки от BiocParallel `txtProgressBar`:**
BiocParallel вызывает два `cat("\n")` при завершении бара — один в `step()` когда
`ntasks == max`, второй в `close(txt)` → `kill()`. Итог: одна лишняя пустая строка.
Фикс (был реализован и отменён вместе со всем параллельным прогресс-кодом):
`cat("\033[1A\033[2K")` сразу после параллельного вызова — ANSI Move Up + Erase Line.

**Потенциальные подходы к параллельному прогрессу (ещё не пробовались):**
- `bpprogressbar=TRUE` + `bptasks=nrow(A)` + `cat("\033[1A\033[2K")` —
  самый простой, использует встроенный механизм BiocParallel.
- Нативная интеграция `progressr` с BiocParallel через `progressr::handlers("txtprogressbar")`
  или будущий BiocParallel-хэндлер в progressr (следить за progressr changelog).
- Кастомный `bpprogressbar`-обработчик через S4-класс (сложно, но правильно).

### Что было сделано в devel-fancy-progress (май–июнь 2026)

**SerialParam — отполировано, слито в devel:**
1. `cli_progress_along()` с `clear = FALSE` — бар остаётся виден после завершения.
2. `format_done = "{cli::pb_name}{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed}"` —
   вместо "ETA: 0s" при завершении показывается затраченное время.
3. Убран дублирующий `cli_progress_step("Identifying friends...")` в `friends.test.r`,
   который открывался до `if/else` и снова внутри parallel `else` ветки.

**Параллельные бэкенды (MulticoreParam, SnowParam):**
- `bpprogressbar = FALSE` принудительно (в `biocparallel-utils.r`).
- Показывается только текстовая метка шага через `cli_progress_step`.
- Несколько подходов к красивому прогресс-бару опробовано и отвергнуто:
  - per-worker `cli_progress_along` (нет TTY в суб-процессах),
  - `bpprogressbar = TRUE` + ANSI cleanup (двойная строка, нельзя управлять курсором),
  - mcparallel + tempfile polling (слишком сложно).
- **Открытый вопрос**: параллельный прогресс-бар так и не решён.

### zzz.R: приветственное сообщение

- Версионная фраза хранится в `yiddish` как Unicode-escapes в `R/zzz.R`.
- Отображение: VS Code → `intToUtf8(rev(utf8ToInt(yiddish)))`, иначе → `str_rtl(yiddish)`.
- Версия 0.99.20: `"\u05E2\u05E8\u05E9\u05D8\u05E2\u05E8 \u05D6\u05D5\u05DE\u05E2\u05E8-\u05D8\u05D0\u05B8\u05D2"`
  ("ערשטער זומער-טאָג" = "First summer day").

## Команды

```bash
# Из R/friends.test/
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --as-cran friends.test_0.99.20.tar.gz
Rscript -e 'BiocCheck::BiocCheck(".")'
```

## Целевое состояние тестов

FAIL 0 | WARN 0 | SKIP 2 (SnowParam в dev-режиме) | PASS 83

## Bioconductor

- Репо принимает полную историю коммитов — не нужен «один чистый коммит».
- Не переписывать историю после начала ревью.
- Ветка должна называться `devel` (не `master`).
- `~/friends.test` (master) — копия для Bioconductor; синхронизируется вручную.
- После каждой серии изменений: скопировать изменившиеся source-файлы в `~/friends.test`.

## Версия

Текущая: **0.99.20** "First summer day" ("ערשטער זומער-טאָג")
