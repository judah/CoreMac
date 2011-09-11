module System.CoreGraphics.PDF.Document (
                        PDFDocument,
                        PDFDocumentRef,
                        newPDFDocumentWithProvider,
                        newPDFDocumentWithURL,
                        getNumberOfPages, 
                        ) where

import Foreign.Ptr
import Foreign.C

import System.CoreFoundation.Base
import System.CoreFoundation.Internal.TH
import System.CoreGraphics.DataProvider
import System.CoreFoundation.URL

#include <QuartzCore/QuartzCore.h>

declareCFTypeAs "CGPDFDocument" "PDFDocument"

{#fun CGPDFDocumentCreateWithProvider as newPDFDocumentWithProvider
    { withObject* `DataProvider' } -> `PDFDocument' getOwned* #}

{#fun CGPDFDocumentCreateWithURL as newPDFDocumentWithURL
    { withObject* `URL' } -> `PDFDocument' getOwned* #}

{#fun CGPDFDocumentGetNumberOfPages as getNumberOfPages
    { withObject* `PDFDocument' } -> `Int' #}

                                    
