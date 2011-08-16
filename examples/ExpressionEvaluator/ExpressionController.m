//
//  ExpressionController.m
//  ExpressionEvaluator
//
//  Created by Judah Jacobson on 8/10/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "ExpressionController.h"

@implementation ExpressionController

-(void)foo {
    NSLog(@"About to call foo()");
    foo();
    NSLog(@"Called foo(a)");
    foo();
    NSLog(@"Called foo(b)");
    foo();
    NSLog(@"Called foo().");    
}

-(IBAction)evaluate:(id)sender {
    [self performSelectorInBackground:@selector(foo) withObject:nil];
    
    NSString *input = [inputField stringValue];
    
    // Since (NSString *) and CFStringRef are toll-free bridged, processInput
    // can use either type as input or output.
    NSString *output = processInput(input);
    [output autorelease];

    [outputField setString:output];
}


@end
