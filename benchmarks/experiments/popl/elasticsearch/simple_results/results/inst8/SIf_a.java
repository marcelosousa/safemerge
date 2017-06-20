/*
 * Licensed to Elasticsearch under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Elasticsearch licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.elasticsearch.painless.node;

import org.elasticsearch.painless.Definition;
import org.elasticsearch.painless.Variables;
import org.objectweb.asm.Label;
import org.elasticsearch.painless.MethodWriter;

/**
 * Represents an if block.
 */
public final class SIf extends AStatement {

    AExpression condition;
    AStatement ifblock;

    public SIf(int line, int offset, String location, AExpression condition, SBlock ifblock) {
        super(line, offset, location);

        this.condition = condition;
        this.ifblock = ifblock;
    }

    @Override
    AStatement analyze(Variables variables) {
        condition.expected = Definition.BOOLEAN_TYPE;
        condition.analyze(variables);
        condition = condition.cast(variables);

        if (condition.constant != null) {
            throw new IllegalArgumentException(error("Extraneous if statement."));
        }

        if (ifblock == null) {
            throw new IllegalArgumentException(error("Extraneous if statement."));
        }

        ifblock.lastSource = lastSource;
        ifblock.inLoop = inLoop;
        ifblock.lastLoop = lastLoop;

        variables.incrementScope();
        ifblock = ifblock.analyze(variables);
        variables.decrementScope();

        anyContinue = ifblock.anyContinue;
        anyBreak = ifblock.anyBreak;
        statementCount = ifblock.statementCount;

        return this;
    }

    @Override
    void write(MethodWriter writer) {
        writer.writeStatementOffset(offset);

        Label fals = new Label();

        condition.fals = fals;
        condition.write(writer);

        ifblock.continu = continu;
        ifblock.brake = brake;
        ifblock.write(writer);

        writer.mark(fals);
    }
}
