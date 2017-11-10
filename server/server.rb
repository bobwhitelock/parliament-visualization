
require 'sinatra'
require 'sequel'

DB = Sequel.postgres(username: 'postgres')


get '/latest-vote' do
  latest_vote = DB[:votes].order(:date).last
  data_for_vote(latest_vote)
end

get '/vote/:id' do |id|
  vote = DB[:votes][{id: id}]
  data_for_vote(vote)
end

def data_for_vote(vote)
  date = vote[:date]

  # TODO Will not give 100% accurate data, e.g. in situation where an MP has
  # changed party between first of month and date of vote.
  first_of_month = Date.new(date.year, date.month, 1)

  votes = DB[:vote_events].where(
    vote_id: vote[:id]
  ).join(
    :people,
    person_id: :person_id,
    date: first_of_month
  ).to_a

  vote.merge(votes: votes).to_json
end
