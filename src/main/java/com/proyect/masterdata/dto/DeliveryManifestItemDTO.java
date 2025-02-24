package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DeliveryManifestItemDTO {
    private Integer quantity;
    private String skuProduct;
    private String skuInventory;
    private Long orderNumber;
    private String district;
    private String customer;
    private String phone;
}
