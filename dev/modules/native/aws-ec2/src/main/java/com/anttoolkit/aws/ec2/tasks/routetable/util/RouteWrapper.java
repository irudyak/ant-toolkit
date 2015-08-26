package com.anttoolkit.aws.ec2.tasks.routetable.util;

import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

public class RouteWrapper
{
    private String destinationCidrBlock;
    private String gatewayId;
    private String instanceId;
    private String networkInterfaceId;
    private String vpcPeeringConnectionId;

    public void setDestinationCidrBlock(String destinationCidrBlock) {
        this.destinationCidrBlock = destinationCidrBlock;
    }

    public void setGatewayId(String gatewayId) {
        this.gatewayId = gatewayId;
    }

    public void setInstanceId(String instanceId) {
        this.instanceId = instanceId;
    }

    public void setNetworkInterfaceId(String networkInterfaceId) {
        this.networkInterfaceId = networkInterfaceId;
    }

    public void setVpcPeeringConnectionId(String vpcPeeringConnectionId) {
        this.vpcPeeringConnectionId = vpcPeeringConnectionId;
    }

    public void createRoute(String routeTableId, AmazonEC2Client client)
    {
        CreateRouteRequest request = new CreateRouteRequest();

        request.setRouteTableId(routeTableId);

        if (destinationCidrBlock != null)
        {
            request.setDestinationCidrBlock(destinationCidrBlock);
        }

        if (gatewayId != null)
        {
            request.setGatewayId(gatewayId);
        }

        if (instanceId != null)
        {
            request.setInstanceId(instanceId);
        }

        if (networkInterfaceId != null)
        {
            request.setNetworkInterfaceId(networkInterfaceId);
        }

        if (vpcPeeringConnectionId != null)
        {
            request.setVpcPeeringConnectionId(vpcPeeringConnectionId);
        }

        client.createRoute(request);
    }
}
