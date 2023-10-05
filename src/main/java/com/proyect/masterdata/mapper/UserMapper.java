package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.request.RequestUserSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface UserMapper {
    UserMapper INSTANCE = Mappers.getMapper(UserMapper.class);
    @Mapping(target = "user",source = "user")
    @Mapping(target = "name",source = "name")
    @Mapping(target = "surname",source = "surname")
    @Mapping(target = "dni",source = "dni")
    @Mapping(target = "email",source = "email")
    @Mapping(target = "address",source = "address")
    @Mapping(target = "gender",source = "gender")
    @Mapping(target = "mobile",source = "mobile")
    @Mapping(target = "password",source = "password")
    @Mapping(target = "status",source = "status")
    @Mapping(target = "district",source = "id_district")
    @Mapping(target = "userType",source = "idUserType")
    @Mapping(target = "modules",ignore = true)
    UserDTO userToUserDTO(User user);
    @Mapping(target = "user",source = "user")
    @Mapping(target = "name",source = "name")
    @Mapping(target = "surname",source = "surname")
    @Mapping(target = "dni",source = "dni")
    @Mapping(target = "email",source = "email")
    @Mapping(target = "address",source = "address")
    @Mapping(target = "gender",source = "gender")
    @Mapping(target = "mobile",source = "mobile")
    @Mapping(target = "password",source = "password")
    @Mapping(target = "status",source = "status")
    @Mapping(target = "id_district",source = "id_district")
    @Mapping(target = "idUserType",source = "idUserType")
    @Mapping(target = "dateRegistration",ignore = true)
    User userToName(RequestUserSave requestUserSave);

    List<User> listUserToListName(List<RequestUserSave> requestUserSaveList);
}
