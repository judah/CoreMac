//
//  CocoaFib.h
//  FibTest
//
//  Created by Judah Jacobson on 8/9/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface CocoaFib : NSObject {
    IBOutlet NSTextField *inputField;
    IBOutlet NSTextField *outputField;
}

-(IBAction)inputChanged:(id)source;

@end
