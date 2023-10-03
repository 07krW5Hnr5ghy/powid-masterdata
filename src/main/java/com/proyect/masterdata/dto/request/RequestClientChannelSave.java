package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestClientChannelSave {
    private String name;
    private String url;
    private boolean status = true;
    private String user;
}
