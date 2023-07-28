package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.response.ResponseDepartment;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IDepartment;
import io.swagger.v3.oas.annotations.Operation;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/department")
@AllArgsConstructor
public class DepartmentController {
    private final IDepartment iDepartment;

    @Operation(summary = "lista los departemanetos ",
            description = "Lista los departamentos maestros")
    @GetMapping()
    public ResponseEntity<List<DepartmentDTO>> listDepartment() throws BadRequestExceptions {
        //throw new BadRequestExceptions("Error datos");
        List<DepartmentDTO> result = iDepartment.listDepartment();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registrar los departemanetos ",
            description = "Registrar los departamentos maestros")
    @PostMapping()
    public ResponseEntity<ResponseDepartment> createDepartment(
            @RequestParam("name") String name) throws BadRequestExceptions {
        //throw new BadRequestExceptions("Error datos");
        ResponseDepartment result = iDepartment.createDepartment(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
