package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestPdfDeliveryManifest {
    private String tokenUser;
    private Long manifestNumber;
    private UUID manifestNumberId;
}
