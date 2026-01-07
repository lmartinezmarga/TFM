# Treball Final del Màster en Ciència de Dades

## Descripció

Aquest repositori conté el desenvolupament del Treball Final de Màster (TFM) titulat **Anàlisi i visualització dels incendis forestals a Catalunya (2000–2024)**.

El projecte se centra en el processament, l’anàlisi i la visualització de dades mitjançant el llenguatge **R**. El flux de treball inclou la gestió de dades crues i processades, diversos scripts d’anàlisi i una aplicació interactiva desenvolupada amb **Shiny** per facilitar l’exploració dels resultats.

A causa de la mida considerable de les dades utilitzades, aquestes no s’allotgen directament al repositori de GitHub, sinó que es proporcionen mitjançant un enllaç extern a Google Drive.

## Autoria

- **Autora del TFM:** Lucía Martinez Margareto
- **Directora del TFM:** Anna Muñoz Bollas
- **Professora responsable:** Susana Acedo Nadal 

## Dades

Les dades originals del projecte es proporcionen a través de Google Drive: [https://drive.google.com/drive/folders/1AouWxMuTUEvxTV-NfEJZJ7awD1bEDihb?usp=drive_link]

Un cop descarregades, les dades s’han col·locar dins la carpeta `dades/`.

Les dades processades no es proporcionen directament, ja que ocupen més espai. Aquestes es generen automàticament executant els scripts d’R inclosos al repositori, a partir de les dades crues.

## Estructura del repositori

> **Notes importants sobre l’organització i execució:**
>  
> - Perquè els scripts funcionin correctament, la carpeta `dades/` ha d’estar situada al mateix nivell que els scripts.
> - Els scripts `.Rmd` es troben al directori principal per facilitar l’accés a la carpeta `shiny/` situada també al mateix nivell. Canviar l’estructura pot afectar les rutes relatives i fer fallar l’execució.
> - Els scripts s’han d’executar en l’ordre indicat per garantir la correcta generació de les dades processades.

El repositori està organitzat de la manera següent:

- **Directori principal: Scripts**  
  - **Processament**  
    - `0-processament-dades.Rmd`
  
  - **Anàlisi**  
    - `01-evolució-temporal.Rmd`  
    - `02-distribució-espacial.Rmd`  
    - `03-cremes-i-incendis.Rmd`  
    - `04-meteorologia.Rmd`  
    - `05-relleu.Rmd`  
    - `06-cobertes.Rmd`  

  Els scripts d’anàlisi parteixen de les dades ja processades i, en alguns casos, apliquen transformacions addicionals específiques per a cada estudi.

- **`html/`**  
  Per a cada script es proporciona una versió en format **HTML**, que permet consultar directament els resultats i les visualitzacions sense necessitat d’executar tot el procés de càlcul.

- **`shiny/`**  
  Carpeta que conté l’aplicació Shiny del projecte:
  
  - `app.R` – Fitxer principal de l’aplicació  
  - `data/` – Dades necessàries perquè l’aplicació funcioni correctament  

## Execució de l’aplicació Shiny

Per executar l’aplicació Shiny localment:

1. Descarregueu la carpeta `shiny/`.
2. Obriu el fitxer `app.R` amb **RStudio**.
3. Executeu l’aplicació amb el botó **Run App** o mitjançant la consola d’R.

Alternativament, l’aplicació també es pot consultar en línia a través del següent enllaç: [https://lmartinezmarga.shinyapps.io/tfm-dashboard/]

## Notes addicionals

- El repositori inclou únicament el codi i els resultats necessaris per garantir la reproductibilitat del projecte.
- Les dades originals no s’inclouen directament al repositori per limitacions d’emmagatzematge.
