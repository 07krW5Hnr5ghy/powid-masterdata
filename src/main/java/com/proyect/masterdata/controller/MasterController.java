package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.services.IPaymentMethod;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/")
@AllArgsConstructor
public class MasterController {

    private final IDepartment iDepartment;
    private final IPaymentMethod iPaymentMethod;

    @Operation(summary = "lista los departemanetos ",
        description = "Lista los departamentos maestros")
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

    @GetMapping(value = "/department")
    public ResponseEntity<List<DepartmentDTO>> listDepartment() throws BadRequestExceptions {
        //throw new BadRequestExceptions("Error datos");
        List<DepartmentDTO> result = iDepartment.listDepartment();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/payment-method")
    public ResponseEntity<List<PaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions{
        List<PaymentMethodDTO>  result = iPaymentMethod.listPaymentMethod();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
