package com.proyect.masterdata.dto;

import com.proyect.masterdata.domain.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ClientChannelsDTO {
    String ecommerceName;
    String url;
    String name;
    User string;
    Boolean status;
}
