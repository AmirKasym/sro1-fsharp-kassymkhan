// ===== Готовый фрагмент: чтение CSV (скопируйте без изменений) =====
// readRows "data.csv" — читает файл рядом со скриптом,
// пропускает строку заголовков и пустые строки,
// возвращает список строк; каждая строка — массив ячеек.
let readRows (fileName: string) =
    let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)
    System.IO.File.ReadAllLines path
    |> Array.toList
    |> List.tail
    |> List.filter (fun line -> line.Trim() <> "")
    |> List.map (fun line ->
        line.Split ';' |> Array.map (fun cell -> cell.Trim()))

// Число с точкой или запятой: "3.5" и "3,5" -> 3.5
let toFloat (text: string) = float (text.Replace(',', '.'))

// Дата в виде "2026-10-05" или "05.10.2026" -> System.DateTime
let toDate (text: string) =
    System.DateTime.ParseExact(text, [| "yyyy-MM-dd"; "dd.MM.yyyy" |], null,
                               System.Globalization.DateTimeStyles.None)
// Дата для печати: showDate d -> "05.10.2026"
let showDate (date: System.DateTime) = date.ToString "dd.MM.yyyy"
// ===== Конец готового фрагмента =====

type Trip =
    { Date: System.DateTime
      Time: string
      Route: string
      Kind: string
      WaitMin: int
      RideMin: int
      Cost: int }

// Превращает массив ячеек одной строки CSV в запись Trip
let parseTrip (cells: string[]) =
    { Date = toDate cells[0]
      Time = cells[1]
      Route = cells[2]
      Kind = cells[3]
      WaitMin = int cells[4]
      RideMin = int cells[5]
      Cost = int cells[6] }

let trips = readRows "data.csv" |> List.map parseTrip

printfn $"Прочитано поездок: {List.length trips}"

// Считает общее время поездки: ожидание плюс время в пути
let totalMinutes (trip: Trip) = trip.WaitMin + trip.RideMin

// Возвращает число поездок, сумму денег и общее время в дороге в часах
let summary (trips: Trip list) =
    let count = List.length trips
    let money = trips |> List.sumBy (fun t -> t.Cost)
    let hours = float (trips |> List.sumBy totalMinutes) / 60.0
    (count, money, hours)

// Находит среднее ожидание для каждого маршрута, от большего к меньшему
let averageWaitByRoute (trips: Trip list) =
    trips
    |> List.groupBy (fun t -> t.Route)
    |> List.map (fun (route, group) ->
        (route, group |> List.averageBy (fun t -> float t.WaitMin)))
    |> List.sortByDescending snd

// Считает, сколько процентов всего времени в дороге уходит на ожидание
let waitShare (trips: Trip list) =
    let waitTotal = trips |> List.sumBy (fun t -> t.WaitMin)
    let allTotal = trips |> List.sumBy totalMinutes
    float waitTotal / float allTotal * 100.0

let (count, money, hours) = summary trips
printfn $"Поездок: {count}, потрачено: {money} ₸, в дороге: %.1f{hours} ч"
printfn "Среднее ожидание по маршрутам:"
averageWaitByRoute trips
|> List.iter (fun (route, wait) -> printfn $"  {route}: %.1f{wait} мин")
printfn $"Доля ожидания: %.1f{waitShare trips} %%"
