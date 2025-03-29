package com.proyect.masterdata.dto.projections;

import java.util.UUID;

public interface DeliveryManifestItemProjection {
    UUID getDeliveryManifestItemId();
    String getUsername();
    Long getManifestNumber();
    String getPhone();
    String getDistrictName();
    Long getOrderNumber();
    UUID getProductId();
    Boolean getDelivered();
    Integer getQuantity();
    String getManagementType();
    String getPaymentMethod();
    String getPaymentState();
    UUID getOrderId();
    UUID getOrderItemId();
    String getCustomerName();
}
