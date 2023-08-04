package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IDepartment;
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

    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iDepartment.save(name,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/departments")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestParam("user") String user,
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iDepartment.saveAll(names, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<DepartmentDTO> update(
            @RequestBody() RequestDepartment requestDepartment
    ) throws BadRequestExceptions {
        DepartmentDTO result = iDepartment.update(requestDepartment);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iDepartment.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/departments")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestParam("user") String user,
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iDepartment.deleteAll(codes, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<DepartmentDTO>> list() throws BadRequestExceptions {
        List<DepartmentDTO> result = iDepartment.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse")
    public ResponseEntity<List<DepartmentDTO>> listStatusFalse() throws BadRequestExceptions {
        List<DepartmentDTO> result = iDepartment.listStatusFalse();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<DepartmentDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        DepartmentDTO result = iDepartment.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<DepartmentDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        DepartmentDTO result = iDepartment.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/user")
    public ResponseEntity<List<DepartmentDTO>> findByUser(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<DepartmentDTO> result = iDepartment.findByUser(user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
/*
    @Operation(summary = "Lista los departmentos",
            description = "Lista los departamentos")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Success",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))}),
            @ApiResponse(responseCode = "400", description = "Bad Request",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "401", description = "Unauthorized",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "403", description = "ForbiddenForbidden",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "404", description = "Not Found",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "409", description = "Conflict",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))}),
            @ApiResponse(responseCode = "500", description = "Internal Server Error",
                    content = { @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponse.class))})
    })

}*/
