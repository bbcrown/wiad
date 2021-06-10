#' Downloads and installs the WIAD Example Dataset
#'
#' The WIAD Example Dataset can be downloaded from https://wiad.science/WIAD_ExampleData.zip .
#' This function downloads and extracts the WIAD Example Dataset in the package folder.
#'
#' @param mode \code{"install"} (default),\code{"update"} or \code{"remove"}
#' @param downloadPath custom location to temporarily store WIAD example dataset while downloading. Default: \code{tempdir()}
#' @param installPath custom location to place WIAD example dataset. Default: WIAD package directory
#' @param dataset  choose to download the \code{"small"} example dataset (248 MB) or the  \code{"full"} example dataset (1.83 GB). Default: \code{"small"}
#' @return No return value
#' @importFrom utils download.file
#' @export
#'
getExampleData <-function(mode = "install",
                          downloadPath = tempdir(),
                          installPath = "",
                          dataset = "small"){

  # URLS and PATHS
  if (
    dataset ==
      "small"
  ) {
    url <-
      "https://wiad.science/WIAD_ExampleData_small.zip"
    msg<-"Downloading small WIAD Example Datset (248 MB).
        This may take a while...\n
        To get the full WIAD Example Datset (1.83 GB) run getExampleData('update',dataset='full')\n"

    localzipfile <-
      file.path(
        downloadPath,
        "WIAD_ExampleData_small.zip"
      )

  } else {
    url <-
      "https://wiad.science/WIAD_ExampleData.zip"
    msg<-"Downloading full WIAD Example Datset (1.83 GB).
        This may take a while...\n"

    localzipfile <-
      file.path(
        downloadPath,
        "WIAD_ExampleData.zip"
      )

  }

  if (
    installPath ==
    ""
  )
    installPath  <-
    system.file(
      package
      =
        "wiad"
    ) # DEFINE PACKAGE NAME HERE

  exampleDataPath <-
    file.path(
      installPath,
      "ExampleData"
    )

  if (mode
      %in%
      c(
        "install",
        "update"
      )
  )
  {

    # Download is started only if /ExampleDataset path does not yet exist or if mode=="update"
    if (
      !dir.exists(
        exampleDataPath
      )
      ||
      mode
      ==
      "update"
    )
    {

      # DOWNLOAD
      message(
        msg
        )


      if(
        file.exists(
          localzipfile
        )
        &&
        mode
        !=
        'update'
        )
        url =
          'http://example.com/' # for testing routines

      dl <-
        try(
          download.file(
            url =
              url,
            destfile =
              localzipfile,
            quiet =
              FALSE,
            mode = "wb"
          )
        )

      if(
        !is.null(attr(dl, "class"))
        &&
        attr(dl, "class") ==
        "try-error"
      ){
        message("Unable to download data for URL:", url, "\n"); return()
      }

      if (
        file.exists(
          localzipfile
          )
        )
        {
        # UNZIP
        message (
          "Extracting WIAD Example Dataset\n"
          )

        utils::unzip(
          zipfile =
            localzipfile,
          exdir =
            installPath,
          overwrite =
            TRUE)

        # CLEANUP
        message (
          "Cleanup\n"
          )

        unlink(
          x =
            localzipfile,
          recursive =
            FALSE
          )

        message(
          "sucessfully installed WIAD Example Dataset to ",
          exampleDataPath,
          '\n')

      } else {
        message ('Unable to extract WIAD Example Dataset: file not found\n')
      }
    } else {
      message(
        'WIAD Example Dataset is already installed. Set mode="update" to reinstall the dataset\n'
      )
    }

    # DO STUFF HERE TO SET AS ACTIVE DATASET

  } else if (
    mode
    ==
    "remove"
  ){
    # ATTENTION: Will remove the example dataset folder without further warning
    if (
      dir.exists(
        exampleDataPath
      )
    ) {

      unlink(
        exampleDataPath,
        recursive =
          TRUE)

      message(
        'Sucessfully removed WIAD Example Dataset\n'
      )

    }else{
      message(
        'Cannot remove WIAD Example Dataset: Dataset not found\n'
      )
    }
  }

}
