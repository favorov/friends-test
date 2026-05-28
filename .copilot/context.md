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

### Тесты параллельности и dev-режим

`test_local()` / `pkgload::load_all()` загружает пакет только в память текущей сессии,
но не в `.libPaths()`. SnowParam-воркеры не могут его найти в dev-режиме.
Решение: `skip_if(pkgload::is_dev_package("friends.test"), ...)` в SnowParam-тестах.
MulticoreParam-тесты работают в dev-режиме (fork наследует память родителя),
но пропускаются на Windows: `skip_on_os("windows")`.

### Broken \link{} references

При удалении публичной функции из API нужно также убрать все `\link{ИМЯ}` в roxygen-комментариях
других файлов — иначе `R CMD check` даёт WARNING.

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
