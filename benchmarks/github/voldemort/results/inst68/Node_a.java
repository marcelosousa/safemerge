/*
 * Copyright 2008-2009 LinkedIn, Inc
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package voldemort.cluster;

import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.log4j.Logger;

import voldemort.annotations.concurrency.Threadsafe;
import voldemort.utils.Utils;

import com.google.common.collect.ImmutableList;

/**
 * A node in the voldemort cluster
 * 
 * @author jay
 * 
 */
@Threadsafe
public class Node implements Serializable, Comparable<Node> {

    private static final Logger logger = Logger.getLogger(Node.class.getName());

    private static final long serialVersionUID = 1;
    private final int id;
    private final String host;
    private final int httpPort;
    private final int socketPort;
    private final int adminPort;
    private final List<Integer> partitions;
    private final NodeStatus status;

    public Node(int id, String host, int httpPort, int socketPort, List<Integer> partitions) {
        this(id, host, httpPort, socketPort, -1, partitions, new NodeStatus());
    }

    public Node(int id,
                String host,
                int httpPort,
                int socketPort,
                int adminPort,
                List<Integer> partitions) {
        this(id, host, httpPort, socketPort, adminPort, partitions, new NodeStatus());
    }

    public Node(int id,
                String host,
                int httpPort,
                int socketPort,
                int adminPort,
                List<Integer> partitions,
                NodeStatus status) {
        this.id = id;
        this.host = Utils.notNull(host);
        this.httpPort = httpPort;
        this.socketPort = socketPort;
        this.status = status;
        this.partitions = ImmutableList.copyOf(partitions);

        // fix default value for adminPort if not defined
        if(adminPort == -1) {
            adminPort = socketPort + 1;
            logger.warn("admin-port not defined for node:" + id
                        + " using default value(socket_port + 1):" + adminPort);
        }

        this.adminPort = adminPort;
    }

    public String getHost() {
        return host;
    }

    public int getHttpPort() {
        return httpPort;
    }

    public int getSocketPort() {
        return socketPort;
    }

    public int getAdminPort() {
        return adminPort;
    }

    public int getId() {
        return id;
    }

    public NodeStatus getStatus() {
        return status;
    }

    public List<Integer> getPartitionIds() {
        return partitions;
    }

    public int getNumberOfPartitions() {
        return partitions.size();
    }

    public URI getHttpUrl() {
        try {
            return new URI("http://" + getHost() + ":" + getHttpPort());
        } catch(URISyntaxException e) {
            throw new IllegalStateException("Invalid host format for node " + id + ".", e);
        }
    }

    public URI getSocketUrl() {
        try {
            return new URI("tcp://" + getHost() + ":" + getSocketPort());
        } catch(URISyntaxException e) {
            throw new IllegalStateException("Invalid host format for node " + id + ".", e);
        }
    }

    @Override
    public String toString() {
        return "Node" + getId() + " partitionList:" + partitions;
    }

    @Override
    public boolean equals(Object o) {
        if(this == o)
            return true;
        if(!(o instanceof Node))
            return false;

        Node n = (Node) o;
        return getId() == n.getId();
    }

    @Override
    public int hashCode() {
        return getId();
    }

    public int compareTo(Node other) {
        return new Integer(this.id).compareTo(other.getId());
    }
}