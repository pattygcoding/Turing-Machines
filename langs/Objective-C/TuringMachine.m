#import <Foundation/Foundation.h>

@interface TuringMachine : NSObject

@property (nonatomic, strong) NSMutableArray *tape;
@property (nonatomic) NSInteger head;
@property (nonatomic, strong) NSString *state;
@property (nonatomic, strong) NSDictionary *transitions;

- (void)runMachine;

@end

@implementation TuringMachine

- (instancetype)init {
    self = [super init];
    if (self) {
        // Initialize the tape with some input
        _tape = [NSMutableArray arrayWithArray:@[@"1", @"1", @"0", @"1"]];
        _head = 0;                     // Initial head position
        _state = @"q0";                 // Initial state
        
        // Define the transition table
        _transitions = @{
            @"q0_1": @[@"q1", @"1", @1],  // On state q0 and reading 1 -> move to q1, write 1, move right
            @"q0_0": @[@"q2", @"1", @1],  // On state q0 and reading 0 -> move to q2, write 1, move right
            @"q1_1": @[@"q0", @"1", @1],  // On state q1 and reading 1 -> move to q0, write 1, move right
            @"q1_0": @[@"q1", @"1", @1],  // On state q1 and reading 0 -> stay in q1, write 1, move right
            @"q2_1": @[@"q2", @"1", @1],  // On state q2 and reading 1 -> stay in q2, write 1, move right
            @"q2_0": @[@"HALT", @"0", @0] // On state q2 and reading 0 -> halt
        };
    }
    return self;
}

- (void)runMachine {
    while (true) {
        NSString *symbol = self.head < self.tape.count ? self.tape[self.head] : @"0";

        NSString *key = [NSString stringWithFormat:@"%@_%@", self.state, symbol];
        NSArray *action = self.transitions[key];

        if (!action) {
            NSLog(@"No valid transition found. Halting.");
            break;
        }

        self.state = action[0];
        if (self.head < self.tape.count) {
            self.tape[self.head] = action[1];
        } else {
            [self.tape addObject:action[1]];
        }
        self.head += [action[2] integerValue];

        NSLog(@"State: %@, Tape: %@, Head: %ld", self.state, [self.tape componentsJoinedByString:@""], (long)self.head);

        if ([self.state isEqualToString:@"HALT"]) {
            break;
        }
    }
    
    NSLog(@"Final Tape: %@", [self.tape componentsJoinedByString:@""]);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        TuringMachine *machine = [[TuringMachine alloc] init];
        [machine runMachine];
    }
    return 0;
}
