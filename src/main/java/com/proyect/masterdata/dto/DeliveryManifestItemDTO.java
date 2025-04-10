package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveryManifestItemDTO {
    private UUID id;
    private UUID orderId;
    private String user;
    private Integer quantity;
    private String skuProduct;
    private String product;
    private Long orderNumber;
    private Long manifestNumber;
    private Integer deliveredQuantity;
    private Integer collectedQuantity;
    private String district;
    private String customer;
    private String phone;
    private String management;
    private String paymentMethod;
    private String paymentState;
    private Double orderItemAmount;
}
