# Накопленный контекст проекта friends.test

Рабочие правила — в `.claude/rules/`. Здесь только знание о проекте.
Этот файл самодостаточен: в нём сведено то, что было в `.copilot/context.md`,
плюс то, что выяснилось в сессиях Claude.

## Архитектура пакета

- **Строки** матрицы A — кандидаты в маркеры (гены, элементы).
- **Столбцы** — кандидаты в «друзья» (паттерны, клеточные типы, коллекции).
- Общий первый шаг: `row.int.ranks()` ранжирует каждый столбец независимо
  через `data.table::frankv(..., ties.method = "random")`.
- Два основных workflow: KS-тест (`friends.test`) и байесовский BIC
  (`friends.test.bic`).
- Оба возвращают одинаковый формат: вложенный именованный список,
  листья — `c(marker, friend, rank)`.
- Параллельность через `BiocParallel`, opt-in через параметр `BPPARAM`,
  по умолчанию `SerialParam()`.

## Ключевые технические решения

### SnowParam и .libPaths() (май 2026)

**Проблема**: `R CMD build` устанавливает пакет во временную директорию,
добавляет её в `.libPaths()` родительского процесса, но SnowParam-воркеры
запускаются как свежие `Rscript`-процессы и эту директорию не наследуют.
Результат: `Error in loadNamespace(x): there is no package called 'friends.test'`
при сборке виньетки на Windows CI.

**Решение** (два компонента, оба необходимы):
1. `local(..., envir = globalenv())` — замыкание несёт `globalenv()` как среду,
   не пространство имён пакета → десериализация на воркере не требует загрузки
   пакета.
2. `libs = .libPaths()` передаётся через `MoreArgs`, воркер вызывает
   `.libPaths(libs)` перед любым `friends.test::` вызовом → воркер может найти
   пакет.

Реализовано в `R/friends.test.r`, `R/friends.test.bic.r`.

### Иерархия классов BiocParallel (май 2026)

`MulticoreParam` **наследует от `SnowParam`**, то есть
`is(MulticoreParam(), "SnowParam")` == TRUE.
При проверке точного типа backend всегда использовать
`class(BPPARAM) == "SnowParam"`, а не `is(BPPARAM, "SnowParam")`.

### Тесты параллельности и dev-режим

`test_local()` / `pkgload::load_all()` загружает пакет только в память текущей
сессии, но не в `.libPaths()`. SnowParam-воркеры не могут его найти в dev-режиме.
Решение: `skip_if(pkgload::is_dev_package("friends.test"), ...)` в
SnowParam-тестах. MulticoreParam-тесты работают в dev-режиме (fork наследует
память родителя), но пропускаются на Windows: `skip_on_os("windows")`.

### Broken \link{} references

При удалении публичной функции из API нужно также убрать все `\link{ИМЯ}`
в roxygen-комментариях других файлов — иначе `R CMD check` даёт WARNING.

## Прогресс-бары: что решено и что тупик

**SerialParam — отполировано, слито в devel:**
1. `cli_progress_along()` с `clear = FALSE` — бар остаётся виден после завершения.
2. `format_done = "{cli::pb_name}{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed}"`
   — вместо "ETA: 0s" при завершении показывается затраченное время.
3. Убран дублирующий `cli_progress_step("Identifying friends...")`
   в `friends.test.r`.

**Параллельные бэкенды (MulticoreParam, SnowParam):**
- `bpprogressbar = FALSE` принудительно (в `biocparallel-utils.r`).
- Показывается только текстовая метка шага через `cli_progress_step`.
- **Открытый вопрос**: красивый параллельный прогресс-бар так и не решён.

**Отвергнутые подходы** (не повторять):
- `progressr::with_progress` + `handler_cli()` с передачей `progressor`
  аргументом в воркеры — ветка `devel-fancy-progress`. Не отображается в
  VS Code R-терминале (не настоящий TTY) и даёт **5x замедление**:
  IPC-overhead от тысяч `p()` вызовов из форкнутых воркеров.
  Вывод: передача `progressor` в воркеры BiocParallel нежизнеспособна.
- per-worker `cli_progress_along` — нет TTY в суб-процессах.
- `bpprogressbar = TRUE` + ANSI cleanup — двойная строка, курсором не управлять.
- mcparallel + tempfile polling — слишком сложно.

## zzz.R: приветственное сообщение

Исторически версионная фраза на идише хранилась как Unicode-escapes и
отображалась через `rtlr::str_rtl` (или реверс строки в VS Code).
Коммит `973e4f0` на `devel` это убрал вместе с зависимостью `rtlr`
(см. «Ревью Bioconductor» ниже).

## Команды

```bash
# Из R/friends.test/
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --as-cran friends.test_0.99.20.tar.gz
Rscript -e 'BiocCheck::BiocCheck(".")'
```

Целевое состояние тестов: FAIL 0 | WARN 0 | SKIP 2 (SnowParam в dev-режиме) |
PASS 83. Целевое состояние проверки: FAIL 0, WARNING 0, не более 2 NOTE.

Линт настроен в `.lintr`: отступ 4, `object_name_linter` и
`object_length_linter` выключены (иначе точки в именах ловятся),
`cyclocomp` до 60.

## Состояние репозиториев (на 5 августа 2026)

Три состояния рассинхронизированы:

| Где | Что |
|---|---|
| `~/friends-test`, локальный `master` @ `c06fab4` | рабочее дерево |
| `origin/devel`, на 2 коммита впереди | `af627ed` context.md, `973e4f0` правки под ревью |
| `~/friends.test` (копия для Bioc), `master` @ `05a30c8` | вмёржен PR #1 от ревьюера |

Локальная ветка — `master`, хотя по правилам рабочая должна быть `devel`.

`diff -rq` между `~/friends-test/R/friends.test` и `~/friends.test` расходится в:
`DESCRIPTION`, `R/friends.test.r`, `R/friends.test.bic.r`,
`R/friends.test.cogaps.example.r`, `R/row.int.ranks.r`,
`vignettes/friends.test.Rmd`.
Только в `~/friends.test`: `.Rbuildignore`, `.github`, `.gitignore`.

## Ревью Bioconductor

Ревьюер — **Hugo Gruson** (`Bisaloo`, `git@hugogruson.fr`). Отдельного треда с
замечаниями нет: ревью пришло кодом, как PR #1 в `~/friends.test`
(вмёржен `05a30c8`, 30 июня 2026), 4 коммита:

- `8026898` Remove unnecessary or auto generated fields in DESCRIPTION
- `84f0c78` Fix typos — `friends.test.r`, `friends.test.bic.r`,
  `row.int.ranks.r`, `friends.test.cogaps.example.r`, виньетка
- `426eecc` Use standard way to specify date in vignette — `date:` в YAML-шапке
  вместо inline `format(Sys.Date(), ...)` в теле
- `6625e89` Remove code blocks indentation — виньетка

**Эти правки есть только в `~/friends.test`** и в основную разработку не
перенесены. Перенос — против обычного направления синхронизации.

Ответ на ревью частично сделан в `973e4f0` на `origin/devel` (1 июля 2026),
локально не подтянут. В нём:
- Title → `Rank-Based Method for Feature Selection in Interaction Matrices`
- Description переписан
- убраны `Date` и `Packaged`
- убрана зависимость `rtlr`, `zzz.R` упрощён
- в README добавлена секция Installation через `BiocManager::install`

Пересекается с `8026898` — при слиянии проверить, не разъехались ли.

### Общие правила Bioconductor

- Репо принимает полную историю коммитов — «один чистый коммит» не нужен.
- Не переписывать историю после начала ревью.
- Ветка должна называться `devel` (не `master`).
- `~/friends.test` — копия для Bioconductor, синхронизируется вручную (`cp`),
  после чего проверяется `diff -rq`. Только по явной команде.

## Наблюдения по коду (кандидаты на правку, не сделано)

- **`R/unif.ks.test.r:26–50` — мёртвый дублирующий код.** Первый блок считает
  `jranks_mapped`, вызывает `ks.test`, а `res$p.value` пишет в никуда. Дальше
  `ranks` перезаписывается новым `jitter()`, но `right_end <- max(jranks)` берёт
  максимум от **первого** джиттера. Возвращается результат второго теста.
  Похоже на остаток от 0.99.13 («KS on ranks mapped to 0..1»).
- **`options(cli.progress_show_after = 0)` без восстановления** —
  `friends.test.r:125`, `friends.test.bic.r:98`. Опция пользователя меняется
  навсегда; нужен `on.exit()`. Классическая придирка на ревью Bioc.
- **`ft_bpparam(.progress =)` ни на что не влияет** — `bpprogressbar` всегда
  `FALSE`, а `@param .progress` в обеих главных функциях до сих пор обещает
  «enable the text progress bar of the selected BPPARAM».
- **Самоссылки `friends.test::`** — в воркерах нужны (там
  `local(envir = globalenv())`), но в последовательном коде
  (`friends.test.r:131`, `friends.test.bic.r:103`) лишние, BiocCheck это метит.
- **8 строк > 80 символов** в `R/`, много в виньетке — NOTE от BiocCheck.
- **Точки в именах функций** (`friends.test`, `best.step.fit`) против
  camelCase-гайдлайна Bioc. Переделка ломающая, отдельно обсудить.

## Версия

Текущая: **0.99.20**. Следующая версия — по советам ревьюера.
