package com.proyect.masterdata.mocks;

import com.proyect.masterdata.dto.ModuleDTO;
import lombok.Getter;

public class ModuleMocks {
    private ModuleDTO module1 = ModuleDTO.builder()
            .moduleName("ventas")
            .modulePrice(34.05)
            .status(true)
            .build();
    private ModuleDTO module2 = ModuleDTO.builder()
            .moduleName("operaciones")
            .modulePrice(10.09)
            .status(true)
            .build();

    private ModuleDTO module3 = ModuleDTO.builder()
            .moduleName("marketing")
            .modulePrice(28.02)
            .status(true)
            .build();

    private ModuleDTO module4 = ModuleDTO.builder()
            .moduleName("finanzas")
            .modulePrice(45.06)
            .status(true)
            .build();
    @Getter
    private ModuleDTO[] moduleListDTO = {module1,module2,module3,module4};
}
