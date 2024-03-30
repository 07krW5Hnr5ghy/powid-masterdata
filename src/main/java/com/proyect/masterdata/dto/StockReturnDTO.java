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
public class StockReturnDTO {
    private String serial;
    private String purchaseSerial;
    private Date registrationDate;
    private Date updateDate;
    private String supplier;
    private Long id;
}
