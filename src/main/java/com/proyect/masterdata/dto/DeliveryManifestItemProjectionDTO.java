package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveryManifestItemProjectionDTO {
    UUID deliveryManifestItemId;
    String username;
    Long manifestNumber;
    String phone;
    String districtName;
    Long orderNumber;
    UUID productId;
    Integer quantity;
    String managementType;
    String paymentMethod;
    String paymentState;
    UUID orderId;
    UUID orderItemId;
    String customerName;
    Integer deliveredQuantity;
    Integer collectedQuantity;
    Integer deliveredProducts;
    Integer itemQuantity;
    String discountName;
    Integer preparedProducts;
    Double discountAmount;
    OffsetDateTime registrationDate;
    String dni;
    String address;
}
