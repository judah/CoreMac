//
//  CocoaFib.m
//  FibTest
//
//  Created by Judah Jacobson on 8/9/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "CocoaFib.h"

@implementation CocoaFib

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}

-(void)awakeFromNib {
    outputField.stringValue = @"";
}

-(IBAction)inputChanged:(id)source {
    NSLog(@"In inputCHanged");
    int i = inputField.intValue;
    outputField.intValue = 2+i;
}


@end
