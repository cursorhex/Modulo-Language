# NoName 
This new language doesnt have a name yet, so idk.
Yea, half of this is AI generated i think.
> i need an idea for a name rn..


# MyLang 🚀

Un linguaggio di programmazione moderno con sezioni modulari e variabili tipizzate.

## Indice
- [Installazione](#installazione)
- [Quick Start](#quick-start)
- [Sintassi Base](#sintassi-base)
- [Sezioni](#sezioni)
- [Variabili](#variabili)
- [Operatori](#operatori)
- [Input/Output](#inputoutput)
- [Program Configuration](#program-configuration)

---

## Quick Start

Crea un file `hello.myl`:

```
section Main{{  
io.print HelloWorld  
}}

program.run [order: {Main}]

```

Esegui:
```
./mylang --i hello.myl
```
 

---

## Sintassi Base

### Commenti
```
// Questo è un commento su singola riga
```
 

### Variabili

#### Variabili Mutabili Locali

```
var(x): 10  
(x): 20 // Modifica il valore  
(x): (x)++ // Incrementa di 1
```
 

#### Costanti Immutabili Locali
```
const(y): 30  
// (y): 40 // ❌ ERRORE: non puoi mutare una costante
```
 
#### Variabili e costanti Locali e Globali

**Variabili/Costanti Locali (`var`, `const`):**
- Esistono **solo all'interno della sezione** in cui sono dichiarate
- Vengono **eliminate automaticamente** alla fine della sezione
- **NON sono accessibili** da altre sezioni

**Variabili Globali (`VAR`, `CONST`):**
- Esistono per **tutta l'esecuzione del programma**
- Sono **accessibili da tutte le sezioni**
- Persistono tra chiamate multiple alla stessa sezione

**Esempio:**
```
section A{{
var(x): 10 // Locale - muore dopo section A
VAR(y): 20 // Globale - vive per sempre
}}

section B{{
io.print (x) // ❌ ERRORE: x non esiste qui!
io.print (y) // ✅ OK: y è globale
}}

program.run [order: {A, B}]
```
### Operatori Aritmetici

```
var(a): 10  
var(b): 5

var(sum): (a) + (b) // 15  
var(diff): (a) - (b) // 5  
var(prod): (a) * (b) // 50  
var(quot): (a) / (b) // 2
```
 

### Incremento
```
var(counter): 0  
(counter): (counter)++ // counter diventa 1
```
 

---

## Sezioni

Le sezioni permettono di organizzare il codice in blocchi riutilizzabili.

### Sintassi

```
section NomeSezione{{  
// codice qui  
}}
```
 

### Esempio

```
section Initialization{{  
var(x): 0  
var(y): 0  
}}

section Processing{{  
(x): (x) + 10  
(y): (y) + 20  
}}

section Output{{  
io.print (x)  
io.print (y)  
}}

program.run [  
    order: {  
        Initialization,  
        Processing,  
        Output  
    }  
]
```
 
**Output:**  

> 10  
> 20

 

---

## Input/Output

### Libreria `io`

#### `io.print`

##### Stampa Valore (con parentesi)

```
var(x): 42  
io.print (x) // Output: 42  
io.print (10 + 5) // Output: 15
```
 

##### Stampa Stringa Letterale (senza parentesi)
```
io.print HelloWorld // Output: HelloWorld  
io.print x // Output: x (non il valore!)
```
 

---

## Program Configuration

Il blocco `program.run` controlla l'esecuzione del programma.

### Opzioni Disponibili
```
program.run [  
    order: {Section1, Section2}, // Ordine di esecuzione  
    mode: release, // release | debug  
    optimize: speed, // speed | size  
    repeat: 5, // Ripete N volte  
    parallel: true, // Esegue in parallelo  
    timeout: 1000, // Timeout in millisecondi  
    onerror: continue, // continue | stop  
    trace: false // Stampa debug info  
]

```

### Esempio Completo
```
section Setup{{  
VAR(counter): 0  
}}

section Loop{{  
(counter): (counter)++  
io.print (counter)  
}}

program.run [  
order: {Setup, Loop},  
repeat: 3,  
mode: debug  
]
```
 

**Output:**  
> 1  
> 2  
> 3

 
---

## Esempi Avanzati

### Calcolo Matematico
```
section Calculate{{  
    const(a): 10  
    const(b): 20  
    var(result): ((a) + (b)) * 2  
    io.print (result) // 60  
}}

program.run [order: {Calculate}]
```
 

### Mutazioni e Incrementi
```
var(x): 10  
const(y): 20

io.print ((x) + (y)) // 30  
io.print y // y  
io.print (y) // 20

(x): 30  
io.print (x) // 30  
io.print x // x

(x): (x)++  
io.print (x) // 31

(x): (x)++  
io.print (x) // 32

io.print ((x) * (x)) // 1024
```
 

---

## Error Handling

### Mutazione di Costanti

```
const(PI): 3.14  
(PI): 3.15 // ❌ Error: Cannot mutate constant 'PI'
```
 

### Variabile Non Trovata
```
io.print (undefinedVar) // ❌ Error: Variable 'undefinedVar' not found
```
 

---

## Best Practices

1. **Usa `const` quando possibile** - Le costanti prevengono errori di mutazione accidentale  
2. **Organizza con sezioni** - Mantieni il codice modulare e leggibile  
3. **Usa nomi descrittivi** - `userAge` è meglio di `x`  
4. **Commenta il codice complesso** - Aiuta te stesso e gli altri  
5. **Variabili globali con parsimonia** - Usa `VAR`/`CONST` solo quando necessario
6.  **Distingui locale da globale** - Usa `var`/`const` per scope limitato, `VAR`/`CONST` per condividere tra sezioni

---

## Roadmap

- [ ] Funzioni custom  
- [ ] Cicli (for, while)  
- [ ] Condizionali (if, else)  
- [ ] Array e Liste  
- [ ] Stringhe e interpolazione  
- [ ] Moduli e import  
- [ ] Error handling avanzato

---

## Contribuire

Pull request benvenute! Per modifiche importanti, apri prima un issue.

## Licenza

MIT
