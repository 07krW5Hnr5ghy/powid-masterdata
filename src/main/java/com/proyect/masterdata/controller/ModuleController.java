package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mocks.ModuleMocks;
import com.proyect.masterdata.mocks.PaymentMocks;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/modules")
@AllArgsConstructor
public class ModuleController {
    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ModuleDTO>> listModules(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ModuleMocks moduleMocks = new ModuleMocks();
        List<ModuleDTO> moduleList = Arrays.asList(moduleMocks.getModuleListDTO());
        return new ResponseEntity<>(moduleList, HttpStatus.OK);
    }
}
