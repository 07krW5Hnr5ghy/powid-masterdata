package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mocks.UserMocks;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/users")
@AllArgsConstructor
public class UserController {
    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<UserDTO>> listUsers(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        UserMocks userMocks = new UserMocks();
        List<UserDTO> userList = Arrays.asList(userMocks.getUserList());
        return new ResponseEntity<>(userList, HttpStatus.OK);
    }
}
