# Installation

Öffnen der Solution in MonoDevelop. Das heisst File > Open und dann
FemtoML.sln auswählen.

## Installation des NuGet-Addons (falls noch nicht installiert)

![Install Nuget Schritt 1](Docs/FemtoML-Install-Nuget.png "Install Nuget Schritt 1")

![Install Nuget Schritt 2](Docs/FemtoML-Install-Nuget2.png "Install Nuget Schritt 2")

## Installation von FParsec in der Solution

![Install FParsec Schritt 1](Docs/FemtoML-Nuget-Menu.png "Install FParsec Schritt 1")

![Install FParsec Schritt 2](Docs/FemtoML-Install-Fparsec.png "Install FParsec Schritt 2")

Das Programm kann nun kompiliert und gestartet werden.

# Erstellen der Dokumentation

Im "Docs"-Ordner hat es zwei Markdown-Files: Tutorial.md und Presentation.md.

Um daraus das PDF erstellen zu können, wird Pandoc und Latex benötigt.

Die besten Resultate hatte ich beim Generieren auf Windows. Die Installation auf Windows
ist folgendermassen:

* Installation von Miktex (http://miktex.org/) (Komplettinstallation)
* Installation von Pandoc (https://github.com/jgm/pandoc/eleases/tag/1.17.1) Downloads
ganz unten.

Danach ein Terminal starten (das "cmd" auf Windows) und im "Docs"-Verzeichnis folgende Befehle ausführen:

* Für die Präsentation: `pandoc Presentation.md -t beamer -o Presentation.pdf`
* Für das Tutorial: `pandoc Tutorial.md --variable=geometry:a4paper -o Tutorial.pdf`

Wichtig: Auf Windows darf das PDF nicht offen sein, wenn die Befehle ausgefürt werden. Ansonsten
hat der Prozess keine Berechtigung, das File zu schreiben.

Falls das Generieren des Totorials mit folgender Message fehlschlägt:

```
14:08 $ pandoc Tutorial.md --variable=geometry:a4paper -o Tutorial.pdf
pandoc: Error producing PDF from TeX source.
! Package babel Error: Unknown option `de'. Either you misspelled it
(babel)                or the language definition file de.ldf was not found.

See the babel package documentation for explanation.
Type  H <return>  for immediate help.
 ...

l.296 \ProcessOptions*
```

dann muss in Tutorial.md im Header "lang: de-CH" auf "lang: german" geändert werden. Das sollte
aber auf Windows nicht passieren.
