package com.proyect.masterdata.dto.projections;

import com.proyect.masterdata.domain.Product;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.util.UUID;

public interface DeliveryManifestItemProjection {
    UUID getDeliveryManifestItemId();
    String getUsername();
    Long getManifestNumber();
    String getPhone();
    String getDistrictName();
    Long getOrderNumber();
    UUID getProductId();
    Integer getQuantity();
    String getManagementType();
    String getPaymentMethod();
    String getPaymentState();
    UUID getOrderId();
    UUID getOrderItemId();
    String getCustomerName();
    Integer getDeliveredQuantity();
    Integer getCollectedQuantity();
    Integer getDeliveredProducts();
    Integer getItemQuantity();
    String getDiscountName();
    Integer getPreparedProducts();
    Double getDiscountAmount();
    String getAddress();
    String getDni();
    Instant getOrdRegistrationDate();
}
