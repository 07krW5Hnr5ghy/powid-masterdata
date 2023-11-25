package com.proyect.masterdata.security;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.List;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.RoleAccess;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
public class SecurityUser implements UserDetails {

    private final User user;
    private final UserRoleRepository userRoleRepository;
    private final RoleRepository roleRepository;
    private final RoleAccessRepository roleAccessRepository;
    private final AccessRepository accessRepository;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {

        List<UserRole> userRoles = userRoleRepository.findByUserId(user.getId());
        List<Role> roles = roleRepository
                .findAllById(userRoles.stream().map(userRole -> userRole.getRoleId()).toList());
        List<GrantedAuthority> authoritiesList = new ArrayList<>();

        for (Role role : roles) {

            authoritiesList.add(new SimpleGrantedAuthority("ROLE:" + role.getName()));

            List<RoleAccess> roleAccesses = roleAccessRepository.findByRoleId(role.getId());

            List<Access> accesses = accessRepository
                    .findAllById(roleAccesses.stream().map(roleAccess -> roleAccess.getAccessId()).toList());

            for (Access access : accesses) {
                authoritiesList.add(new SimpleGrantedAuthority("ACCESS:" + access.getName()));
            }

        }

        return authoritiesList;
    }

    @Override
    public String getPassword() {
        return user.getPassword();
    }

    @Override
    public String getUsername() {
        return user.getUsername();
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return user.getStatus();
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return user.getStatus();
    }

}
