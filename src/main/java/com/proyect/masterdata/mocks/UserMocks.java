package com.proyect.masterdata.mocks;

import com.proyect.masterdata.dto.UserDTO;
import lombok.Getter;

public class UserMocks {
    private String[] modulesList1 = {"ventas","operaciones"};
    private UserDTO user1 = UserDTO.builder()
            .dni("123456789")
            .username("ag65")
            .name("ALEJANDRO")
            .surname("GOMEZ")
            .email("agomes@gmail.com")
            .password("abc123+")
            .district("KUSCO")
            .phoneNumber("123456789")
            .userType("VENDEDOR")
            .gender("M")
            .modules(modulesList1)
            .status(true)
            .build();
    private String[] modulesList2 = {"marketing","finanzas"};
    private UserDTO user2 = UserDTO.builder()
            .dni("123456789")
            .username("gm33")
            .name("GERMAN")
            .surname("MENDEZ")
            .email("g45men@gmail.com")
            .password("123+abc")
            .district("LIMA")
            .phoneNumber("56567897")
            .userType("marketing")
            .gender("M")
            .modules(modulesList2)
            .status(true)
            .build();
    private String[] modulesList3 = {"usuarios","operaciones","finanzas","inventario"};
    private UserDTO user3 = UserDTO.builder()
            .dni("123456789")
            .username("lr66")
            .name("LINA")
            .surname("RODRIGUEZ")
            .email("lina78r@gmail.com")
            .password("abc111+")
            .district("CORDOBA")
            .phoneNumber("56464321")
            .userType("administrador")
            .gender("F")
            .modules(modulesList3)
            .status(true)
            .build();
    private String[] modulesList4 = {"operaciones","inventario","pedidos"};
    private UserDTO user4 = UserDTO.builder()
            .dni("123456789")
            .username("ccc45")
            .name("CAROLINA")
            .surname("CASAS")
            .email("cc120@gmail.com")
            .password("+abc233")
            .district("CALLAO")
            .phoneNumber("987654321")
            .userType("operaciones")
            .gender("F")
            .modules(modulesList4)
            .status(true)
            .build();
    @Getter
    private UserDTO[] userList = {user1,user2,user3,user4};
}
