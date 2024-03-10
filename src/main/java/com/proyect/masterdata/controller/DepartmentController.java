package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IDepartment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("department")
@AllArgsConstructor
public class DepartmentController {
        private final IDepartment iDepartment;

        @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
        //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DEPARTMENT_POST')")
        public ResponseEntity<ResponseSuccess> save(
                        @RequestParam("name") String name,
                        @RequestParam("user") String user) throws BadRequestExceptions {
                ResponseSuccess result = iDepartment.save(name, user);
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

        @PostMapping(value = "departments", consumes = MediaType.APPLICATION_JSON_VALUE)
        @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DEPARTMENT_POST')")
        public ResponseEntity<ResponseSuccess> saveAll(
                        @RequestParam("user") String user,
                        @RequestBody() List<String> names) throws BadRequestExceptions {
                ResponseSuccess result = iDepartment.saveAll(names, user);
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

        @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
        @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DEPARTMENT_POST')")
        public ResponseEntity<DepartmentDTO> update(
                        @RequestBody() RequestDepartment requestDepartment) throws BadRequestExceptions {
                DepartmentDTO result = iDepartment.update(requestDepartment);
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

        @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
        @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DEPARTMENT_DELETE')")
        public ResponseEntity<ResponseDelete> delete(
                        @RequestParam("code") Long code,
                        @RequestParam("user") String user) throws BadRequestExceptions {
                ResponseDelete result = iDepartment.delete(code, user);
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

        @GetMapping()
        public ResponseEntity<List<DepartmentDTO>> listDepartment() throws BadRequestExceptions {
                List<DepartmentDTO> result = iDepartment.listDepartment();
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

        @GetMapping(value = "list")
        @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DEPARTMENT_GET')")
        public ResponseEntity<Page<DepartmentDTO>> list(
                        @RequestParam(value = "name", required = false) String name,
                        @RequestParam(value = "user", required = false) String user,
                        @RequestParam(value = "sort", required = false) String sort,
                        @RequestParam(value = "sortColumn", required = false) String sortColumn,
                        @RequestParam("pageNumber") Integer pageNumber,
                        @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
                Page<DepartmentDTO> result = iDepartment.list(name, user, sort, sortColumn, pageNumber, pageSize);
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

        @GetMapping(value = "status-false")
        @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:DEPARTMENT_GET')")
        public ResponseEntity<Page<DepartmentDTO>> listStatusFalse(
                        @RequestParam(value = "name", required = false) String name,
                        @RequestParam(value = "user", required = false) String user,
                        @RequestParam(value = "sort", required = false) String sort,
                        @RequestParam(value = "sortColumn", required = false) String sortColumn,
                        @RequestParam("pageNumber") Integer pageNumber,
                        @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
                Page<DepartmentDTO> result = iDepartment.listStatusFalse(name, user, sort, sortColumn, pageNumber,
                                pageSize);
                return new ResponseEntity<>(result, HttpStatus.OK);
        }

}
/*
 * @Operation(summary = "Lista los departmentos",
 * description = "Lista los departamentos")
 * 
 * @ApiResponses(value = {
 * 
 * @ApiResponse(responseCode = "200", description = "Success",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = List.class))}),
 * 
 * @ApiResponse(responseCode = "400", description = "Bad Request",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = ErrorResponse.class))}),
 * 
 * @ApiResponse(responseCode = "401", description = "Unauthorized",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = ErrorResponse.class))}),
 * 
 * @ApiResponse(responseCode = "403", description = "ForbiddenForbidden",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = ErrorResponse.class))}),
 * 
 * @ApiResponse(responseCode = "404", description = "Not Found",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = ErrorResponse.class))}),
 * 
 * @ApiResponse(responseCode = "409", description = "Conflict",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = ErrorResponse.class))}),
 * 
 * @ApiResponse(responseCode = "500", description = "Internal Server Error",
 * content = { @Content(mediaType = "application/json", schema
 * = @Schema(implementation = ErrorResponse.class))})
 * })
 * 
 * }
 */
