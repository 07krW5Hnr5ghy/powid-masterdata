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
    private Integer quantity;
    private String skuProduct;
    private Long orderNumber;
    private Long manifestNumber;
    private String district;
    private String customer;
    private String phone;
    private String management;
}
