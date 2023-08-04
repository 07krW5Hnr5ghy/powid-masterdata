package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.MembershipType;
import com.proyect.masterdata.dto.MembershipTypeDTO;
import com.proyect.masterdata.dto.request.RequestMembershipType;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MembershipTypeMapper {
    MembershipTypeMapper INSTANCE = Mappers.getMapper(MembershipTypeMapper.class);
    @Mapping(target = "code", source = "id")
    MembershipTypeDTO membershipTypeToMembershipTypeDTO(MembershipType membershipType);
    List<MembershipTypeDTO> listMembershipTypeToListMembershipTypeDTO(List<MembershipType> membershipTypeList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    MembershipType membershipTypeToName(String name);

    List<MembershipType> listMembershipTypeToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    MembershipType requestMembershipTypeToMembershipType(RequestMembershipType requestMembershipType);
}
