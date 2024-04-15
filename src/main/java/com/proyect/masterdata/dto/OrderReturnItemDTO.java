package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderReturnItemDTO {
    private Long orderId;
    private String supplierProduct;
    private String productSku;
    private Date registrationDate;
    private Date updateDate;
    private String returnType;
    private String warehouse;
    private Integer quantity;
}
