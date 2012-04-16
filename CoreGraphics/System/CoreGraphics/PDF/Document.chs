module System.CoreGraphics.PDF.Document (
                        PDFDocument,
                        PDFDocumentRef,
                        CGPDFDocument,
                        newPDFDocumentWithProvider,
                        newPDFDocumentWithURL,
                        getNumberOfPages, 
                        ) where

import Foreign.Ptr
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Foreign
import System.CoreFoundation.Internal.TH
{#import System.CoreGraphics.DataProvider#}
import System.CoreFoundation.URL

#include <QuartzCore/QuartzCore.h>

declareCFTypeAs "CGPDFDocument" "PDFDocument"
{#pointer CGPDFDocumentRef as PDFDocumentRef nocode#}

newPDFDocumentWithProvider :: DataProvider -> IO PDFDocument
newPDFDocumentWithProvider dp = getOwned $ cgNewPDFDocumentWithProvider dp

{#fun CGPDFDocumentCreateWithProvider as cgNewPDFDocumentWithProvider
    { withObject* `DataProvider' } -> `PDFDocumentRef' id #}

newPDFDocumentWithURL :: URL -> IO PDFDocument
newPDFDocumentWithURL url = getOwned $ cgNewPDFDocumentWithURL url

{#fun CGPDFDocumentCreateWithURL as cgNewPDFDocumentWithURL
    { withObject* `URL' } -> `PDFDocumentRef' id #}

{#fun CGPDFDocumentGetNumberOfPages as getNumberOfPages
    { withObject* `PDFDocument' } -> `Int' #}

                                    
