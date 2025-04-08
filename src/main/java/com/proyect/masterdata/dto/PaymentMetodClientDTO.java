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
public class PaymentMetodClientDTO {
    private UUID id;
    private String nameAccount;
    private String detailAccount;
    private String observationsAccount;
}
