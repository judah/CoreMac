//
//  ExpressionController.h
//  ExpressionEvaluator
//
//  Created by Judah Jacobson on 8/10/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "haskell/ForeignExpr_stub.h"

@interface ExpressionController : NSObject {
    IBOutlet NSTextField *inputField;
    IBOutlet NSTextView *outputField;
}

-(IBAction)evaluate:(id)sender;

@end
