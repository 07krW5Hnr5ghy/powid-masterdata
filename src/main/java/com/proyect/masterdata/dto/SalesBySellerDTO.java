package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SalesBySellerDTO {
    private String seller;
    private String department;
    private String province;
    private String district;
    private String closingChannel;
    private Long totalOrders;
}
