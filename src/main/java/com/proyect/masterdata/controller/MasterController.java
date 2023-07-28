package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.handler.ErrorResponse;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.services.IPaymentMethod;
import com.proyect.masterdata.services.IPaymentState;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/")
@AllArgsConstructor
public class MasterController {

    private final IDepartment iDepartment;
    private final IPaymentState iPaymentState;

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

    @GetMapping(value = "/payment-state")
    public ResponseEntity<List<PaymentStateDTO>> listPaymentState() throws BadRequestExceptions{
        List<PaymentStateDTO> result = iPaymentState.listPaymentState();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PostMapping(value = "/payment-state-add")
    public ResponseEntity<String> addPaymentState(@RequestBody String paymentState) throws BadRequestExceptions{
        iPaymentState.addPaymentState(paymentState);
        return new ResponseEntity<>("Payment method " + paymentState + " created.",HttpStatus.CREATED);
    }

    @DeleteMapping(value = "/payment-state-delete")
    public ResponseEntity<String> deletePaymentState(@RequestBody Long id) throws BadRequestExceptions{
        iPaymentState.deletePaymentState(id);
        return new ResponseEntity<>("Payment state with id : " + id + " deleted.",HttpStatus.OK);
    }

    @PutMapping(value = "/payment-state-put")
    public ResponseEntity<String> updatePaymentState(@RequestBody PaymentStateDTO data) throws BadRequestExceptions{
        iPaymentState.updatePaymentState(data.getName(), data.getId());
        return new ResponseEntity<>("Payment state with id : " + data.getId() + " change name to " + data.getName() + ".",HttpStatus.OK);
    }

}
