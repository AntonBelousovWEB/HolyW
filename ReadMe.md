# HolyWC - Компилятор из HolyW (HW) в WebAssembly Text (WAT)

## Описание

HolyWC - это компилятор, преобразующий код на языке HW (специально разработанный подмножество языка) в WebAssembly Text Format (WAT). Проект включает:

- Лексический анализатор (лексер)
- Синтаксический анализатор (парсер)
- Генератор кода WAT
- Поддержку импорта внешних функций

## Особенности

- Поддержка функций с параметрами и возвращаемыми значениями
- Арифметические операции (+, -, *, /)
- Локальные переменные и параметры функций
- Импорт внешних функций (например, из JavaScript)
- Генерация оптимизированных имен для WASM

## Требования

- GCC или другой C-компилятор
- Make
- WABT (для тестирования сгенерированного WAT)

## Запуск

```bash
make (clen / translate / wasm / run )
./holywc example.hw example.wat
sudo apt install wabt
wat2wasm example.wat
```