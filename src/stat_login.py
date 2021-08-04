# -*- coding: utf-8 -*-
import scrapy
from scrapy import FormRequest

class StatLoginSpider(scrapy.Spider):
    name = 'stat_login'
    allowed_domains = ['stathead.com']
    start_urls = ['https://stathead.com/users/login.cgi']
    page_count = 0

    def parse(self, response):
        yield FormRequest.from_response(
            response,
            #form_method = '//form',
            formdata = {
                "username": 'Landeros_p33',
                "password": 'p!0pez!ander2733',
                "referrer": 'https%3A%2F%2Fstathead.com%2Fbasketball%2Flineup_finder.cgi%3Frequest%3D1%26match%3Dsingle%26order_by_asc%3D0%26order_by%3Ddiff_pts%26lineup_type%3D5-man%26output%3Dtotal%26is_playoffs%3DN%26year_id%3D2015%26game_month%3D0%26game_num_min%3D0%26game_num_max%3D99',
                "token": '0'
            },
            callback=self.after_login
        )

    def after_login(self,response):
        for player in response.css("div table tbody tr"):
            #rk =   player.css("div div div table tbody tr th::text").get()
            yield {
                #"rk":rk,
                "Rk": player.css("th::text").get(),
                "Lineup": player.css("th+td a::text").get(),
                "Lineup_2": player.css("th+td a+a::text").get(),
                "Lineup_3": player.css("th+td a+a+a::text").get(),
                "Lineup_4": player.css("th+td a+a+a+a::text").get(),
                "Lineup_5": player.css("th+td a+a+a+a+a::text").get(),
                "Tm": player.css("td+td a::text").get(),
                "Season":  player.css("td+td+td a::text").get(),
                "G":  player.css("td+td+td+td::text").get(),
                "MP":  player.css("td+td+td+td+td::text").get(),
                "TM":  player.css("td+td+td+td+td+td::text").get(),
                "OPP":  player.css("td+td+td+td+td+td+td::text").get(),
                "Pace":  player.css("td+td+td+td+td+td+td+td::text").get(),
                "FG":  player.css("td+td+td+td+td+td+td+td+td::text").get(),
                "FGA": player.css("td+td+td+td+td+td+td+td+td+td::text").get(),
                "FG%":  player.css("td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "3P":  player.css("td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "3PA":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "3P%":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "EFG%":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "FT":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "FTA":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "FT%":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td::text").get(),
                "PTS%":  player.css("td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td+td::text").get()
        
            }

    
        next_pag = (f"https://stathead.com/basketball/lineup_finder.cgi?request=1&game_num_max=99&order_by=diff_pts&game_month=0&match=single&is_playoffs=N&output=total&year_id=2015&order_by_asc=0&lineup_type=5-man&game_num_min=0&offset={self.page_count}")
        if next_pag is not None:
             next_pag = response.urljoin(next_pag)
             self.page_count = self.page_count + 100
             yield scrapy.Request(next_pag, callback=self.after_login)

