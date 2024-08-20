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
public class StockTransactionDTO {
    private String serial;
    private String warehouse;
    private String transactionType;
    private Date registrationDate;
}
