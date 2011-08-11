//
//  ExpressionController.m
//  ExpressionEvaluator
//
//  Created by Judah Jacobson on 8/10/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "ExpressionController.h"

@implementation ExpressionController

-(IBAction)evaluate:(id)sender {
    NSString *input = [inputField stringValue];
    
    // Since (NSString *) and CFStringRef are toll-free bridged, processInput
    // can use either type as input or output.
    NSString *output = processInput(input);
    [output autorelease];

    [outputField setString:output];
}


@end
